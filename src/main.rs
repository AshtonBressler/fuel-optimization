use interpolation::Lerp;
use ordered_float::OrderedFloat;
use std::collections::BTreeMap;
use std::f64::{self, consts::E}; 
use std::f64::consts::PI; // Shleby is this right vs: crate::f64::consts::PI; or std::f32::consts::PI;
use std::path::Path;

use std::fs;
use std::fs::File;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

const MACH_ONE: f64 = 343.0;

struct FuelOptimizationProblem {
    gravity: f64,
    rocket_mass: f64,
    cross_sectional_area: f64,
    fuel_efficiency: f64,
    max_throttle_change_rate: f64, // % / s
    // Velocity -> coefficient
    drag_coefficient: BTreeMap<OrderedFloat<f64>, f64>,
    max_drag_coefficient: f64,
    total_pressure: f64, //idk where I got this but it works
    throat_diameter: f64, // 0.344 inch
    exhaust_diameter: f64, // 0.9 inch
    specific_heat_ratio: f64, // estimated specific heat ratio
    total_temp: f64, 
    universal_gas_constant: f64,

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
    fn tick(&mut self, problem: &FuelOptimizationProblem, dt: f64, desired_throttle_opening: f64) {
        let max_throttle_change = problem.max_throttle_change_rate * dt;
        let throttle_opening_delta = (desired_throttle_opening - self.throttle_opening)
            .clamp(-max_throttle_change, max_throttle_change);
        self.throttle_opening += throttle_opening_delta;
        self.total_time += dt;

        self.velocity += self.acceleration(problem) * dt;
        self.position += self.velocity * dt;

        self.fuel_mass -= self.mass_flow_rate(problem) * dt;
        self.fuel_mass = self.fuel_mass.clamp(0.0, f64::MAX);

        self.position = self.position.clamp(0.0, f64::MAX);
        if self.velocity < 0.0 && self.position == 0.0 {
            self.velocity = 0.0;
        }
    }

//////////////////////////////////////////////

    fn total_mass_flow_rate(&self, problem: &FuelOptimizationProblem) -> f64 {
        let throat_area = PI/4.0 * problem.throat_diameter * problem.throat_diameter;
            throat_area
            * problem.total_pressure
            / problem.total_temp.sqrt()
            * problem.specific_heat_ratio.sqrt()
            / (self.gas_constant(problem)).sqrt()
            * f64::powf((problem.specific_heat_ratio + 1.0)/2.0, -(problem.specific_heat_ratio+1.0 )/(2.0 * (problem.specific_heat_ratio-1.0)))
    }

    fn gas_constant(&self, problem: &FuelOptimizationProblem) -> f64 {
            problem.universal_gas_constant/self.molar_mass_exhuast(problem)       
    }
      

    fn oxydizer_mass_flow(&self, problem: &FuelOptimizationProblem) -> f64 {
            self.total_mass_flow_rate(problem) - self.fuel_mass_flow(problem)
    }

    fn fuel_mass_flow(&self, problem: &FuelOptimizationProblem) -> f64 {
        let alpha = 0.7; // guess from research
        let n = 0.75; // guess from research
        let fuel_bore_area = PI/4.0 * (fuel_bore_diameter+2*fuel_regression)*(fuel_bore_diameter+2*fuel_regression);//mm^2
        let fuel_regression_rate = alpha * f64::powf((self.oxydizer_mass_flow(problem)/fuel_bore_area), n); //mm/s
            fuel_density
            * fuel_height
            * (fuel_bore_diameter+2*fuel_regression)*PI
            * fuel_regression_rate
            * time????
    }

///////       
    fn molar_mass_exhuast(&self, problem: &FuelOptimizationProblem) -> f64 {
            // 138.0/5.0  based on guess from C25H52 and NO2

            // for Nitrox at -80c and a 30% by mass oxygen
        let nitrous_molar_mass = 44;
        let oxygen_molar_mass = 32;
        let molar_flow_nitrous = 0.7*self.oxydizer_mass_flow(problem)/nitrous_molar_mass ; 
        let molar_flow_oxygen = 0.3*self.oxydizer_mass_flow(problem)/oxygen_molar_mass ;
        let total_molar_oxygen_flow = molar_flow_oxygen + molar_flow_nitrous ;// one mole of oxygen in one mole of nitrous

            // Al + wax 50/50 by mass (nano aluminum)
        let wax_molar_mass = 353; // parraffin
        let aluminum_molar_mass = 27; 
        let molar_flow_wax =  0.5*self.fuel_mass_flow(problem) / wax_molar_mass;
        let molar_flow_aluminum =  0.5 * self.fuel_mass_flow(problem)/aluminum_molar_mass;  
    
        // combustion output molar masses
        let carbon_dioxide_molar_mass = 44;
        let water_molar_mass = 18;
        let nitrogen_molar_mass = 14;
        let aluminum_oxide_molar_mass = 102;

        let extra_oxygen_molar_flow = total_molar_oxygen_flow - (molar_flow_wax * 38) - (molar_flow_aluminum * (4/3));
        let carbon_dioxide_molar_flow = molar_flow_wax * 25;
        let water_molar_flow = molar_flow_wax * 26;
        let aluminum_oxide_molar_flow = molar_flow_aluminum * 0.5;
        let nitrogen_molar_flow = molar_flow_nitrous/2 ;//0.5 nitrogen mole per nitrous mole

        let exhuast_total_molar_flow = 
            extra_oxygen_molar_flow
            + carbon_dioxide_molar_flow
            + water_molar_flow
            + aluminum_oxide_molar_flow
            + nitrogen_molar_flow

        extra_oxygen_molar_flow/exhuast_total_molar_flow * oxygen_molar_mass
        + carbon_dioxide_molar_flow/exhuast_total_molar_flow * carbon_dioxide_molar_mass
        + water_molar_flow/exhuast_total_molar_flow * water_molar_mass
        + aluminum_oxide_molar_flow/exhuast_total_molar_flow * aluminum_oxide_molar_mass
        + nitrogen_molar_flow/exhuast_total_molar_flow * nitrogen_molar_mass

    }


///////
    fn thrust(&self, problem: &FuelOptimizationProblem) -> f64 {
        let exhust_area = PI/4.0 * problem.exhaust_diameter * problem.exhaust_diameter;
        let exit_mach = 3.15109;  // solved by hand for small rocket test expansion reatio Ae/A = 6.8458
        let exhust_temp = problem.total_temp * f64::powf((1.0 + ((problem.specific_heat_ratio-1.0)/2.0) * exit_mach * exit_mach), -1.0) ;
        let exhust_pressure = problem.total_pressure * f64::powf((1.0 + ((problem.specific_heat_ratio-1.0)/2.0) * exit_mach * exit_mach),(- problem.specific_heat_ratio / (problem.specific_heat_ratio-1.0)));
        let exhust_velocity = exit_mach * (self.gas_constant(problem).sqrt() * exhust_temp * problem.specific_heat_ratio);
          self.mass_flow_rate(problem)
          * exhust_velocity
          + (exhust_pressure - self.free_stream_pressure(problem))
          * exhust_area
    }

    fn free_stream_pressure(&self, problem: &FuelOptimizationProblem) -> f64 {
        problem.total_pressure * (14.7/2000.0) // static for test fire 
    }

//////////////////////////////////////////////

    fn air_density(&self, problem: &FuelOptimizationProblem) -> f64 {
        1.46 * E.powf(-0.000134 * self.position)
    }

    fn air_drag(&self, problem: &FuelOptimizationProblem) -> f64 {
        (problem.cross_sectional_area / 2.0)
            * self.velocity
            * self.velocity
            * self.air_density(problem)
            * problem.drag_coefficient(self.velocity)
    }

    fn acceleration(&self, problem: &FuelOptimizationProblem) -> f64 {
        let total_mass = self.fuel_mass + problem.rocket_mass;
        let gravitational_drag = problem.gravity * total_mass;
        (self.thrust(problem) + gravitational_drag - self.air_drag(problem)) / total_mass
    }

}

fn main() -> Result<()> {
    let map = load_drag_coefficient_map(Path::new("./drag_coefficient_map.txt"))?;
    let hard_coded = {
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
        fuel_efficiency: 300.0, // ISP in seconds
        max_throttle_change_rate: 2.0,
        drag_coefficient: map,
        max_drag_coefficient: 0.3,
        universal_gas_constant: 8.314462,
        total_pressure: 437041.5, //idk where I got this but it works
        throat_diameter: 0.008737, // 0.344 inch
        exhaust_diameter: 0.02286, // 0.9 inch
        specific_heat_ratio: 1.26, // estimated specific heat ratio
        total_temp: 3000.0,
    };
    let initial_state = RocketState {
        fuel_mass: 2.0,
        ..RocketState::default()
    };

    let mut csv_out = String::from("");

    simulate(
        &problem,
        initial_state,
        30.0,
        0.010,
        |_, _| 1.0,
        |state, current_time| {
            let data = format!(
                "{}\t{}\t{}\t{}\t{}\t{}\t{}\n",
                current_time,
                state.acceleration(&problem) / -problem.gravity,
                state.fuel_mass,
                state.position,
                state.air_density(&problem),
                state.velocity,
                state.air_drag(&problem) + (state.fuel_mass * state.acceleration(&problem))
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
    desired_throttle: impl Fn(&FuelOptimizationProblem, &RocketState) -> f64,
    mut on_step: impl FnMut(&RocketState, f64),
) {
    let mut current_time = 0.0;
    let mut state = initial_state;
    while current_time <= total_time {
        on_step(&state, current_time);
        state.tick(&problem, timestep, desired_throttle(problem, &state));

        current_time += timestep;
    }
}
