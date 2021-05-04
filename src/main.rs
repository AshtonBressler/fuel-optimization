use interpolation::Lerp;
use ordered_float::OrderedFloat;
use std::collections::BTreeMap;
use std::cell::Cell;
use std::f64::consts::PI; // Shleby is this right vs: crate::f64::consts::PI; or std::f32::consts::PI;
use std::f64::{self, consts::E};
use std::path::Path;

use std::fs;
//use std::fs::File;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const MACH_ONE: f64 = 343.0;

#[derive(Default, Clone)]
struct FuelOptimizationProblem {
    gravity: f64,
    rocket_mass: f64,
    cross_sectional_area: f64,
    // fuel_efficiency: f64,
    max_throttle_change_rate: f64, // % / s
    // Velocity -> coefficient
    drag_coefficient: BTreeMap<OrderedFloat<f64>, f64>,
    max_drag_coefficient: f64,
    total_pressure: f64,      //idk where I got this but it works
    throat_diameter: f64,     // 0.344 inch
    exhaust_diameter: f64,    // 0.9 inch
    specific_heat_ratio: f64, // estimated specific heat ratio
    total_temp: f64,
    universal_gas_constant: f64,
    fuel_bore_diameter: f64,
    fuel_height: f64,
}

impl FuelOptimizationProblem {
    fn drag_coefficient(&self, velocity: f64) -> f64 {
        let velocity = OrderedFloat(velocity);
        let first_below = self
            .drag_coefficient
            .range(..velocity)
            .next_back()
            .unwrap_or((&OrderedFloat(f64::MIN), &self.max_drag_coefficient));
        let next_above = self
            .drag_coefficient
            .range(velocity..)
            .next()
            .unwrap_or((&OrderedFloat(f64::MAX), &0.0));

        let percent = (velocity.0 - (first_below.0).0) / ((next_above.0).0 - (first_below.0).0);
        first_below.1.lerp(next_above.1, &percent)
    }
}

#[derive(Clone, Default, Debug)]
struct RocketState {
    position: f64,
    velocity: f64,
    fuel_mass: f64,
    throttle_opening: f64,
    total_time: f64,
}

impl RocketState {
    fn air_density(&self) -> f64 {
        1.46 * E.powf(-0.000134 * self.position)
    }
}

fn recursive_call<T>(cell: &Cell<u8>, max_depth: u8, default: T, normal: impl FnOnce() -> T) -> T {
    if cell.get() >= max_depth { return default }
  cell.set(cell.get() + 1);
  
  let result = normal();
  cell.set(cell.get() - 1);
  result
 }

#[derive(Default)]
struct ProblemState {
  state: RocketState,
  problem: FuelOptimizationProblem,
  oxydizer_depth: Cell<u8>,
  gas_constant_depth: Cell<u8>,
  fuel_regression_depth: Cell<u8>,
}

impl ProblemState {
    fn tick(&mut self, dt: f64, desired_throttle_opening: f64) {
        let max_throttle_change = self.problem.max_throttle_change_rate * dt;
        let throttle_opening_delta = (desired_throttle_opening - self.state.throttle_opening)
            .clamp(-max_throttle_change, max_throttle_change);
        self.state.throttle_opening += throttle_opening_delta;
        self.state.total_time += dt;

        self.state.velocity += self.acceleration() * dt;
        self.state.position += self.state.velocity * dt;

        self.state.fuel_mass -= self.fuel_mass_flow(0.01,1.5) * dt;
        self.state.fuel_mass = self.state.fuel_mass.clamp(0.0, f64::MAX);
//dbg!(self.state.fuel_mass); 

        self.state.position = self.state.position.clamp(0.0, f64::MAX);
        if self.state.velocity < 0.0 && self.state.position == 0.0 {
            self.state.velocity = 0.0;
        }
        /////
self.testnan();
        /////
    }

    fn air_drag(&self) -> f64 {
        (self.problem.cross_sectional_area / 2.0)
            * self.state.velocity
            * self.state.velocity
            * self.state.air_density()
            * self.problem.drag_coefficient(self.state.velocity)
    }

    fn acceleration(&self) -> f64 {
        let total_mass = self.state.fuel_mass + self.problem.rocket_mass;
       
        let gravitational_drag = self.problem.gravity * total_mass;
        (self.thrust() + gravitational_drag - self.air_drag()) / total_mass
    }
  
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////
  //////////////////////////////////////////////

    fn total_mass_flow_rate(&self, _min: f64, _max: f64) -> f64 {
        let throat_area = PI / 4.0 * self.problem.throat_diameter * self.problem.throat_diameter;      
        throat_area * self.problem.total_pressure / self.problem.total_temp.sqrt()
            * self.problem.specific_heat_ratio.sqrt() / (self.gas_constant()).sqrt()
            * f64::powf(
                (self.problem.specific_heat_ratio + 1.0) / 2.0,
                -(self.problem.specific_heat_ratio + 1.0) / (2.0 * (self.problem.specific_heat_ratio - 1.0)),
            )       
    }

    fn gas_constant(&self) -> f64 {
        recursive_call(
            &self.gas_constant_depth,
            1,
            286.7, // Air to start (J/kg/K)
            || self.problem.universal_gas_constant / self.molar_mass_exhuast()
            // for molar mass in g/mole gives KJ/Kg/K
        )
    }

    fn oxydizer_mass_flow(&self, _min: f64, _max: f64) -> f64 {
        recursive_call(
            &self.oxydizer_depth,
            1,
            0.1,
            || self.total_mass_flow_rate(0.1,2.0) - self.fuel_mass_flow(0.01,self.total_mass_flow_rate(0.1,2.0))
        )
    }

    fn fuel_regression_rate(&self) -> f64 {
        let alpha = 0.7; // guess from research
        let n = 0.75; // guess from research
        (alpha
            * f64::powf(
                self.oxydizer_mass_flow(0.01,self.total_mass_flow_rate(0.1,2.0)) / self.fuel_bore_area(),
                n,
            ))
            / 1000.0 // m/s
    }

    fn fuel_bore_area(&self) -> f64 {
        PI / 4.0
            * (self.problem.fuel_bore_diameter + 2.0 * self.fuel_regression_total())
            * (self.problem.fuel_bore_diameter + 2.0 * self.fuel_regression_total())
        //mm^2
    }

    fn fuel_regression_total(&self) -> f64 {
        recursive_call(
            &self.fuel_regression_depth,
            1,
            0.0001, // in meters = 0.1mm regressed 
            || self.fuel_regression_rate() * self.state.total_time
        )
    }

    fn fuel_mass_flow(&self, _min: f64, _max: f64) -> f64 {
        let fuel_density = (2712.0 + 900.0) / 2.0; // for 50/50 aluminum to wax (kg/m^3)
        fuel_density
            * self.problem.fuel_height
            * (self.problem.fuel_bore_diameter + 2.0 * self.fuel_regression_total())
            * PI
            * self.fuel_regression_rate()
    }

    ///////
    fn molar_mass_exhuast(&self) -> f64 {
        //molar mass in kg/mol for math

        // for Nitrox at -80c and a 30% by mass oxygen
        let nitrous_molar_mass = 44.0/1000.0; 
        let oxygen_molar_mass = 32.0/1000.0;
        let molar_flow_nitrous = 0.7 * self.oxydizer_mass_flow(0.01,self.total_mass_flow_rate(0.1,2.0)) / nitrous_molar_mass;
        let molar_flow_oxygen = 0.3 * self.oxydizer_mass_flow(0.01,self.total_mass_flow_rate(0.1,2.0)) / oxygen_molar_mass;
        let total_molar_oxygen_flow = molar_flow_oxygen + molar_flow_nitrous; // one mole of oxygen in one mole of nitrous

        // Al + wax 50/50 by mass (nano aluminum)
        let wax_molar_mass = 353.0/1000.0; // parraffin
        let aluminum_molar_mass = 27.0/1000.0;
        let molar_flow_wax = 0.5 * self.fuel_mass_flow(0.001,self.total_mass_flow_rate(0.1,2.0)) / wax_molar_mass;
        let molar_flow_aluminum = 0.5 * self.fuel_mass_flow(0.01,self.total_mass_flow_rate(0.1,2.0)) / aluminum_molar_mass;

        // combustion output molar masses
        let carbon_dioxide_molar_mass = 44.0/1000.0;
        let water_molar_mass = 18.0/1000.0;
        let nitrogen_molar_mass = 14.0/1000.0;
        let aluminum_oxide_molar_mass = 102.0/1000.0;

        let extra_oxygen_molar_flow =
            total_molar_oxygen_flow - (molar_flow_wax * 38.0) - (molar_flow_aluminum * (4.0 / 3.0));
        let carbon_dioxide_molar_flow = molar_flow_wax * 25.0;
        let water_molar_flow = molar_flow_wax * 26.0;
        let aluminum_oxide_molar_flow = molar_flow_aluminum * 0.5;
        let nitrogen_molar_flow = molar_flow_nitrous / 2.0; //0.5 nitrogen mole per nitrous mole

        let exhuast_total_molar_flow = extra_oxygen_molar_flow
            + carbon_dioxide_molar_flow
            + water_molar_flow
            + aluminum_oxide_molar_flow
            + nitrogen_molar_flow;

        extra_oxygen_molar_flow / exhuast_total_molar_flow * oxygen_molar_mass
            + carbon_dioxide_molar_flow / exhuast_total_molar_flow * carbon_dioxide_molar_mass
            + water_molar_flow / exhuast_total_molar_flow * water_molar_mass
            + aluminum_oxide_molar_flow / exhuast_total_molar_flow * aluminum_oxide_molar_mass
            + nitrogen_molar_flow / exhuast_total_molar_flow * nitrogen_molar_mass

        // 29.0/1000

    }

    ///////
    fn thrust(&self) -> f64 {
        let exhust_area = PI / 4.0 * self.problem.exhaust_diameter * self.problem.exhaust_diameter;
        let exit_mach = 3.15109; // solved by hand for small rocket test expansion reatio Ae/A = 6.8458
        let exhust_temp = self.problem.total_temp
            * f64::powf(
                1.0 + ((self.problem.specific_heat_ratio - 1.0) / 2.0) * exit_mach * exit_mach,
                -1.0,
            );
        let exhust_pressure = self.problem.total_pressure
            * f64::powf(
                1.0 + ((self.problem.specific_heat_ratio - 1.0) / 2.0) * exit_mach * exit_mach,
                -self.problem.specific_heat_ratio / (self.problem.specific_heat_ratio - 1.0),
            );          

        let exhust_velocity = exit_mach
            * (self.gas_constant().sqrt() * exhust_temp * self.problem.specific_heat_ratio);

// assert!(!exhust_velocity.is_nan());

        self.total_mass_flow_rate(0.1,2.0) * exhust_velocity
            + (exhust_pressure - self.free_stream_pressure()) * exhust_area
    }


    fn free_stream_pressure(&self) -> f64 {
        self.problem.total_pressure * (14.7 / 2000.0)// static for test fire
    }
//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////

    fn testnan(&self) -> f64 {
    let nantest = self.air_drag();
    dbg!(nantest);  
    // assert!(!nantest.is_nan());
    nantest
    }

//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////
//////////////////////////////////////////////

} 


fn main() -> Result<()> {  

    let map = load_drag_coefficient_map(Path::new("./drag_coefficient_map.txt"))?;
    let _hard_coded = {
        let mut map = BTreeMap::new();
        map.insert(OrderedFloat(0.0), 0.3);
        map.insert(OrderedFloat(1.7 * MACH_ONE), 0.15);
        map.insert(OrderedFloat(5.0 * MACH_ONE), 0.07);
        map
    };
    let problem = FuelOptimizationProblem {
        gravity: -9.8,
        rocket_mass: 0.10,
        cross_sectional_area: 0.050,
        // fuel_efficiency: 300.0, // ISP in seconds
        max_throttle_change_rate: 2.0,
        drag_coefficient: map,
        max_drag_coefficient: 0.3,
        universal_gas_constant: 8.314462, // J/ K / mol
        total_pressure: 13789514.58,  // pascals // 2000psi 
        throat_diameter: 0.008737, // 0.344 inch
        exhaust_diameter: 0.02286, // 0.9 inch
        specific_heat_ratio: 1.26, // estimated specific heat ratio "y"
        total_temp: 3000.0, // Kelvin (K)
        fuel_bore_diameter: 0.015, // Meters
        fuel_height: 0.200, // Meters
    };
    let initial_state = RocketState {
        fuel_mass: 2.0,
        ..RocketState::default()
    };

    let mut csv_out = String::from("");

    simulate(
        &problem,
        initial_state,
        2.0,
        0.10,
        |_| 1.0,
        |problem_state, current_time| {
            let data = format!(
                //////////////////// Just making the spot where I change simulations output easier to find ////////////////
                "{}\t{}\n",
                current_time,
                problem_state.acceleration(),
            );
            csv_out.push_str(&data)
        },
    );
    fs::write("./data/test2.txt", csv_out).expect("Unable to write file");
    Ok(())
}

fn load_drag_coefficient_map(path: &Path) -> Result<BTreeMap<OrderedFloat<f64>, f64>> {
    let mut reader = csv::Reader::from_path(path)?;
    let map = reader
        .records()
        .map(|result| parse_floats(&result?[0]))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .step_by(6)
        .map(|floats| (OrderedFloat(floats[0] * MACH_ONE), floats[2]))
        .collect::<BTreeMap<_, _>>();
    Ok(map)
}

fn parse_floats(record: &str) -> Result<Vec<f64>> {
    record
        .split_whitespace()
        .map(|s| {
            s.parse::<f64>()
                .map_err(|err| Box::new(err) as Box<dyn std::error::Error>)
        })
        .collect()
}

fn simulate(
    problem: &FuelOptimizationProblem,
    initial_state: RocketState,
    total_time: f64,
    timestep: f64,
    desired_throttle: impl Fn(&ProblemState) -> f64,
    mut on_step: impl FnMut(&ProblemState, f64),
) {
    let mut current_time = 0.0;
    let mut problem_state = ProblemState { state: initial_state, problem: problem.clone(), ..Default::default() };
  
    while dbg!(total_time - current_time) > 0.0 {
        on_step(&problem_state, current_time);
        problem_state.tick(timestep, desired_throttle(&problem_state));
        current_time += timestep;
    }
}