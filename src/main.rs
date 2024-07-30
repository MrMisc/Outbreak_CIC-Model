// use std::thread;
use rand::distributions::Uniform;
use rand::distributions::{Distribution, Standard};
use rand::prelude::SliceRandom;
use rand::{thread_rng, Rng};
use statrs::distribution::{Gamma, Normal, Poisson, StudentsT, Triangular, Weibull};
extern crate rayon;
use rayon::prelude::*;

extern crate serde;
// extern crate serde_json;
// use serde::Deserializer;
// use serde::{Deserialize, Serialize};
// use serde_json::json;

// use std::error::Error;
use std::fs::File;
use std::fs::OpenOptions;
// use std::io::Read;
use std::io::Write;
// use std::time::{Duration, Instant};
use std::{fs, io, process};
// use std::error::Error;
use csv::Writer;

pub mod limits {
    pub fn min(a: f64, b: f64) -> f64 {
        if a < b {
            a
        } else {
            b
        }
    }
    pub fn max(a: f64, b: f64) -> f64 {
        if a < b {
            b
        } else {
            a
        }
    }
}

pub fn poisson(rate: f64) -> u64 {
    let mut rng = thread_rng();
    let v: &Vec<f64> = &Poisson::new(rate)
        .unwrap()
        .sample_iter(&mut rng)
        .take(1)
        .collect();
    v[0] as u64
}

pub fn normal(mean: f64, std: f64, upper: f64) -> f64 {
    let mut thing: f64 = 0.0;
    loop {
        // println!("Mean value is {}, STD value is {}",mean,std);
        let mut rng = thread_rng();
        let v: &Vec<f64> = &Normal::new(limits::max(0.0001, mean), limits::max(0.0001, std))
            .unwrap()
            .sample_iter(&mut rng)
            .take(1)
            .collect();
        thing = v[0];
        if thing > 0.0 && thing < upper {
            break;
        }
    }
    thing
}

pub fn normal_(min: f64, mean: f64, std: f64, upper: f64) -> f64 {
    let mut thing: f64 = 0.0;
    loop {
        // println!("Mean value is {}, STD value is {}",mean,std);
        let mut rng = thread_rng();
        let v: &Vec<f64> = &Normal::new(limits::max(0.0001, mean), limits::max(0.0001, std))
            .unwrap()
            .sample_iter(&mut rng)
            .take(1)
            .collect();
        thing = v[0];
        if thing > min && thing < upper {
            break;
        }
    }
    thing
}

pub fn gamma(alpha: f64, beta: f64) -> f64 {
    let mut rng = thread_rng();
    let v: &Vec<f64> = &Gamma::new(alpha, beta)
        .unwrap()
        .sample_iter(&mut rng)
        .take(1)
        .collect();
    v[0]
}

pub fn roll(prob: f64) -> bool {
    let mut rng = thread_rng();
    let roll = Uniform::new(0.0, 1.0);
    let rollnumber: f64 = rng.sample(roll);
    rollnumber < prob
}

pub fn uniform(start: f64, end: f64) -> f64 {
    let mut rng = thread_rng();
    let roll = Uniform::new(start, end);
    let rollnumber: f64 = rng.sample(roll);
    rollnumber
}

#[derive(Clone)]
pub struct Zone_3D {
    segments: Vec<Segment_3D>,
    zone: usize,
    capacity: u32,
    eviscerate: bool,
}

#[derive(Clone)]
pub struct Segment_3D {
    zone: usize,
    origin_x: u64,
    origin_y: u64,
    origin_z: u64,
    range_x: u64,
    range_y: u64,
    range_z: u64,
    capacity: u32,
    eviscerated: bool,
}

impl Segment_3D {
    fn generate(&mut self, infected: bool, colonized: bool, mut n: u32, mut hosts: &mut Vec<host>) {
        //fn new(zone:usize, std:f64,loc_x:f64, loc_y:f64,loc_z:f64,restriction:bool,range_x:u64,range_y:u64,range_z:u64, atc0:f64, atc1:f64,probability:f64)->host{
        if n < self.capacity {
            self.capacity -= n;
        }
        if !infected {
            hosts.push(host::new(
                self.zone,
                CONTACT_TRANSMISSION_PROBABILITY[self.zone],
                self.origin_x as f64,
                self.origin_y as f64,
                self.origin_z as f64,
                RESTRICTION,
                self.range_x,
                self.range_y,
                self.range_z,
            ));
        } else {
            let mut host_to_add: host = host::new_inf(
                self.zone,
                CONTACT_TRANSMISSION_PROBABILITY[self.zone],
                self.origin_x as f64,
                self.origin_y as f64,
                self.origin_z as f64,
                RESTRICTION,
                self.range_x,
                self.range_y,
                self.range_z,
            );
            host_to_add.colonized = colonized;
            hosts.push(host_to_add);
        }
        // new_segment
    }
}

#[derive(Clone, Debug)]
pub struct Eviscerator {
    zone: usize,
    set: usize,
    infected: bool,
    chronology: usize,
    number_of_times_infected: u8,
}

impl Zone_3D {
    fn add(&mut self) -> [u64; 7] {
        // println!("Adding to {}",self.zone);
        //Arbitrary numbers that I set beforehand. Simply
        let mut origin_x: u64 = 200000;
        let mut origin_y: u64 = 200000;
        let mut origin_z: u64 = 200000;
        let mut range_x: u64 = 0;
        let mut range_y: u64 = 0;
        let mut range_z: u64 = 0;
        self.segments
            .sort_by(|segment_a, segment_b| segment_b.capacity.cmp(&segment_a.capacity)); //sort vector by capacity first -> Compare accordingly so that
        if let Some(first_space) = self
            .segments
            .iter_mut()
            .find(|segment| segment.capacity >= 1)
        {
            //0 occupants
            //Segment capacity update
            first_space.capacity -= 1; //add 1 occupant
            origin_x = first_space.origin_x;
            origin_y = first_space.origin_y;
            origin_z = first_space.origin_z;
            range_x = first_space.range_x;
            range_y = first_space.range_y;
            range_z = first_space.range_z;
        }
        let condition: bool = origin_x != 200000 && origin_y != 200000 && origin_z != 200000;
        if condition {
            self.capacity -= 1;
        }
        [
            condition as u64,
            origin_x,
            origin_y,
            origin_z,
            range_x,
            range_y,
            range_z,
        ]
    }
    fn subtract(&mut self, x: u64, y: u64, z: u64) {
        let mut counter: bool = false;
        // println!("Reached subtract function!");
        self.segments.iter_mut().for_each(|mut seg| {
            // println!("x vs segment x is {} vs {}",x,seg.origin_x);
            if x == seg.origin_x && y == seg.origin_y && z == seg.origin_z {
                // println!("Reaching inside of subtract function here!");
                seg.capacity += 1;
                self.capacity += 1;
                counter = true;
            }
        });
    }
    fn generate_empty(zone: usize, grid: [u64; 3], step: [usize; 3]) -> Zone_3D {
        let mut vector: Vec<Segment_3D> = Vec::new();
        let mut count: u32 = 0;
        for x in (0..grid[0]).step_by(step[0]) {
            for y in (0..grid[1]).step_by(step[1]) {
                for z in (0..grid[2]).step_by(step[2]) {
                    vector.push(Segment_3D {
                        zone: zone.clone(),
                        origin_x: x,
                        origin_y: y,
                        origin_z: z,
                        range_x: step.clone()[0] as u64,
                        range_y: step.clone()[1] as u64,
                        range_z: step.clone()[2] as u64,
                        capacity: NO_OF_HOSTS_PER_SEGMENT[zone] as u32,
                        eviscerated: false,
                    });
                    count += NO_OF_HOSTS_PER_SEGMENT[zone] as u32;
                }
            }
        }
        Zone_3D {
            segments: vector,
            zone: zone,
            capacity: count,
            eviscerate: EVISCERATE_ZONES.contains(&zone),
        }
    }
    fn generate_full(zone: usize, grid: [u64; 3], step: [usize; 3]) -> Zone_3D {
        let mut vector: Vec<Segment_3D> = Vec::new();
        for x in (0..grid[0]).step_by(step[0]) {
            for y in (0..grid[1]).step_by(step[1]) {
                for z in (0..grid[2]).step_by(step[2]) {
                    vector.push(Segment_3D {
                        zone: zone.clone(),
                        origin_x: x,
                        origin_y: y,
                        origin_z: z,
                        range_x: step.clone()[0] as u64,
                        range_y: step.clone()[1] as u64,
                        range_z: step.clone()[2] as u64,
                        capacity: 0,
                        eviscerated: false,
                    })
                }
            }
        }
        Zone_3D {
            segments: vector,
            zone: zone,
            capacity: 0,
            eviscerate: EVISCERATE_ZONES.contains(&zone),
        }
    }
    fn feed_setup(self, vector: Vec<host>, time: usize) -> Vec<host> {
        let mut vec: Vec<host> = vector.clone();
        // println!("HERE");
        for segment in self.clone().segments {
            host::feed(
                &mut vec,
                segment.origin_x.clone(),
                segment.origin_y.clone(),
                segment.origin_z.clone(),
                segment.zone.clone(),
                time,
            );
        }
        vec
    }

    fn modify(&mut self, start: [u64; 3], end: [u64; 3], range: [usize; 3]) {
        //range is synonymous with the stepsize in side STEP -> we are modifying the step here ---> hetero dims will not be reflected visually
        self.segments.retain(|segment| {
            start
                .iter()
                .zip([segment.origin_x, segment.origin_y, segment.origin_z].iter())
                .all(|(&a, &b)| a <= b)
                == false
                && end
                    .iter()
                    .zip([segment.origin_x, segment.origin_y, segment.origin_z].iter())
                    .all(|(&a, &b)| a >= b)
                    == false
        });

        for x in (start[0]..end[0]).step_by(range[0]) {
            for y in (start[1]..end[1]).step_by(range[1]) {
                for z in (start[2]..end[2]).step_by(range[2]) {
                    self.segments.push(Segment_3D {
                        zone: self.zone,
                        origin_x: x,
                        origin_y: y,
                        origin_z: z,
                        range_x: range.clone()[0] as u64,
                        range_y: range.clone()[1] as u64,
                        range_z: range.clone()[2] as u64,
                        capacity: self.segments[0].capacity,
                        eviscerated: self.eviscerate,
                    })
                }
            }
        }
    }
    fn multi_eviscerate(
        &mut self,
        eviscerators: &mut Vec<Eviscerator>,
        vector1: &mut Vec<host>,
        time: usize,
        counter: usize,
        no_to_do: usize,
        line_no: usize,
    ) {
        // Sort eviscerators by chronology
        eviscerators.sort_by_key(|ev| ev.chronology);
        vector1.sort_by(|a, b| a.origin_x.cmp(&b.origin_x));
        let mut processed_eviscerators = Vec::new(); // Vector to hold processed eviscerators
                                                     // println!("Zone of the hosts here are ")
        for line in 0..line_no {
            let mut vector: Vec<&mut host> = vector1
                .iter_mut()
                .filter(|host| !host.eviscerated && host.zone == self.zone && host.motile == 0)
                .take(no_to_do)
                .collect();
            for chronology in 0..=NO_OF_EVISCERATORS - 1 {
                let mut eviscerators_for_this_chronology = eviscerators
                    .iter()
                    .filter(|ev| {
                        ev.chronology == chronology && ev.set == line && ev.zone == self.zone
                    })
                    .cloned()
                    .collect::<Vec<_>>();

                // println!("We have {} probes on this eviscerator to process this, and {} of them are contaminated",eviscerators_for_this_chronology.clone().len() as u64 ,eviscerators_for_this_chronology.clone().into_par_iter().filter(|x| x.infected).collect::<Vec<_>>().len() as u64);
                // println!("We have {} contaminated hosts before processing via {}th eviscerator",vector1.clone().into_par_iter().filter(|x| x.motile == 0 && x.contaminated).collect::<Vec<_>>().len() as u64,chronology);
                // Process hosts with eviscerators of the current chronology
                let evis_after = self.eviscerate(
                    &mut eviscerators_for_this_chronology,
                    &mut vector,
                    time,
                    line,
                );

                // Record the processed eviscerators
                processed_eviscerators.extend_from_slice(&evis_after);
                // println!("We have {} contaminated hosts after processing via {}th eviscerator",vector1.clone().into_par_iter().filter(|x| x.motile == 0 && x.contaminated).collect::<Vec<_>>().len() as u64,chronology);
                // Optionally, update vector1 with the processed hosts
                // vector1 = processed_hosts;
            }
            for host in vector.iter_mut() {
                host.eviscerated = true; // Assuming 'eviscerated' is a boolean field in your Host struct
            }
        }

        // Update the original eviscerators vector with the processed ones
        // println!("Eviscerators before {:?}", eviscerators.clone().into_par_iter().filter(|x| x.infected).collect::<Vec<_>>().len() as u64);
        *eviscerators = processed_eviscerators.clone();
        // println!("Eviscerators after {:?}",processed_eviscerators.clone().into_par_iter().filter(|x| x.infected).collect::<Vec<_>>().len() as u64);
    }

    fn eviscerate(
        &mut self,
        evs: &mut Vec<Eviscerator>,
        vector: &mut Vec<&mut host>,
        time: usize,
        set: usize,
    ) -> Vec<Eviscerator> {
        //SETUP
        // println!("Are we even eviscerating?");
        // println!("Current zone is {}", self.zone);
        // println!("Number to do is {}", no_to_do);
        // let mut vector: Vec<&mut host> = vector1.iter_mut().filter(|host| !host.eviscerated && host.zone == self.zone && host.motile == 0).take(no_to_do).collect();
        // let mut vector: Vec<&mut host> = vector1.iter_mut().filter(|host| !host.eviscerated && host.motile == 0).take(no_to_do).collect();
        // println!("Length of relevant vector hosts is {}", vector.len());
        // let cloned_vector: Vec<host> = vector.iter().map(|host_ref| host_ref.clone()).collect();
        // println!("We have {} hosts before to eviscerate, {} of them are contaminated",vector.iter().filter(|&x| {
        //     !x.eviscerated
        // }).collect::<Vec<_>>().len(), vector.iter().filter(|&x| {
        //     x.contaminated
        // }).collect::<Vec<_>>().len());

        let vector_length: usize = vector.len();
        let no_infected: usize = vector
            .iter()
            .filter(|&x| x.infected)
            .collect::<Vec<_>>()
            .len();
        let no_not_infected: usize = vector
            .iter()
            .filter(|&x| !x.infected)
            .collect::<Vec<_>>()
            .len();
        // println!("[BEFORE] : {} of {} hosts in this evisceration process, at zone {} are not-infected, while {} are infected",no_not_infected,vector_length,self.zone, no_infected);

        //Filter out eviscerators that are for the zone in particular
        // let mut evs: Vec<&mut Eviscerator> = eviscerators.iter_mut().filter(|ev| ev.zone == self.zone && ev.set == set).collect();
        // Define the step size for comparison
        let step_size = NO_OF_PROBES[self.zone - 1];

        fn radialise(ang: f64, dist: f64, d: f64) -> bool {
            //Function to convert the radial distance between 2 points into a linear distance (minimum distance between these 2 points, and compare it )
            let mut min: f64 = 2.0 * (1.0 - ang.cos());
            min = min.powf(0.5) * (dist) / ang;
            min < d
        }

        // Decision:If the mishap explosion does not even exceed the spacing between the evisceration belt in side the eviscerationi zone, we do not need to bother doing mishap explosions
        //if CURVATURE is false -> all calculations below for minimum distance follow the typical self.segments[0].range_xand linear calculations propagate forward
        let have_curvature: f64 = (CURVATURE as u64) as f64;
        //unit angle between stations of hosts in evisceration process
        let angle: f64 =
            2.0 * (PI / (step_size as f64 - 1.0) * have_curvature + PI * (1.0 - have_curvature));
        //Cosine rule to calculate c from a,b and angle AB between 2 nearest ( this and next line)
        let mut minimum_distance: f64 = 2.0 * (1.0 - angle.cos());
        minimum_distance = minimum_distance.powf(0.5) * (self.segments[0].range_x as f64) / angle;
        //Way to remove this curvature calculation if CURVATURE IS FALSE -> relegating calculation to just the second linear term instead
        minimum_distance = minimum_distance * have_curvature
            + (self.segments[0].range_x as f64) * (1.0 - have_curvature);

        //Create a variable that denotes the number of indices/segment units as spaces
        let mut index_spacing: usize = 0;
        //If we are dealing with a simple straight configuration...
        // println!("Calculating index spacing with MISHAP RADIUS: {}, and segmental spacing being {}", MISHAP_RADIUS,self.segments[0].range_x);
        index_spacing = (MISHAP_RADIUS as u64 / self.segments[0].range_x) as usize;
        // println!("So index spacing being calculated ends up being {}",index_spacing);
        //Indices of all hosts that undergo MISHAP is inside list of indices ind
        //Note that in CURVATURE model, if the stations are positioned adequately away angularly, there may very well be blockage from that angle -> shielding from MISHAP
        //We use a user defined variable -> ANGLE MAXIMA to denote the maximum angualr displacement under which MISHAPS can be spread
        // let mut no:usize = 0;
        if CURVATURE {
            // println!("angle is {} degrees", angle*180.0/PI);
            // println!("While ANGLE_MAXIMA (the numerator) is {} degrees", ANGLE_MAXIMA*180.0/PI);
            //In curvature terms, we limit it using ANGLE MAZXIMA
            // println!("ANGLE MAXIMA IS {} while angle is {}", ANGLE_MAXIMA, angle);
            let mut angle_spacing: usize = 0;
            if ANGLE_MAXIMA <= PI {
                angle_spacing = ((ANGLE_MAXIMA / angle) as u64) as usize;
            } else {
                angle_spacing = ((PI / angle) as u64) as usize;
            }
            // println!("Previous index spacing is {}", index_spacing);
            if index_spacing > angle_spacing {
                index_spacing = angle_spacing;
            }
            // println!("As a result, index spacing has been changed to {}", index_spacing);
            // panic!("Checking angles");
            // index_spacing = no.clone();
        }
        // println!("[POST CURVATURE ADJUSTMENT] So index spacing being calculated ends up being {}",index_spacing);

        let mut ind: Vec<usize> = Vec::new();
        //Range to be affected, with respect to each origin point
        let mut revised_ind: Vec<[usize; 4]> = Vec::new();

        if MISHAP && MISHAP_RADIUS > minimum_distance {
            vector.iter_mut().enumerate().for_each(|(idx, mut host)| {
                if (host.infected || host.contaminated) && roll(MISHAP_PROBABILITY) {
                    //Assume that only a previously infected host that has been brought to the evisceration process exploding, can pose a threat of  spreading the infection -> a fresh uninfected chicken that has been simultaneously infected and exploded
                    //by the probe, should not intuitively count as a mishap infection event
                    ind.push(idx);
                }
            });
            vector.sort_by(|a, b| a.origin_x.cmp(&b.origin_x));
            for &idx in &ind {
                //THERE IS SOMETHING WRONG WITH HOW INDICES ARE DETEREMINED HERE
                //Criteria for determining start and end index of affected hosts by the mishap explosion
                //pre-emptively calculate it ot be within the bounds of vector

                //NOTE idx should be the vector wide index number, NOT the relative index number within  each set of eviscerators
                // println!("INDEX IS {}",idx);
                let internal_index: usize = idx % step_size;
                let mut minima: usize = idx - idx % step_size;
                let mut maxima: usize = idx - idx % step_size + step_size;
                // if minima<0{minima = 0;}
                if maxima > vector.len() {
                    maxima = vector.len();
                }
                //Remember that index spacing is the maximum number of indices that the index movement can happen
                //So let us simplify matters whereby we consider RELATIVE INDICES WITHIN EACH SET OF EVISCERATORS NOT the actual index -> internal_index (range being [0,step_size])
                // println!("Starting relative index is {}, while index spacing calculated is {}",internal_index, index_spacing);
                let mut start_index = if internal_index >= index_spacing {
                    internal_index - index_spacing
                } else {
                    0
                };
                let mut end_index = std::cmp::min(internal_index + index_spacing, step_size);
                // println!("[INITIAL] Start and End indices of {} and {} respectively",start_index,end_index);
                if CURVATURE {
                    // println!("Conducting CURVATURE PROCEDURE");
                    //Curvature has a tendency to compress distances together, not lengthen. In other words, we can simply check to see if the remaining excluded stations are within range now
                    //Edit start_index -> See if extendable
                    while start_index > 0
                        && radialise(
                            ((internal_index - start_index) as f64 / (step_size as f64) * PI),
                            (((internal_index - start_index) as u64) * self.segments[0].range_x)
                                as f64,
                            MISHAP_RADIUS,
                        )
                    {
                        start_index -= 1;
                    }
                    // Increase end_index if extendable
                    while end_index < step_size
                        && radialise(
                            ((end_index - internal_index) as f64 / (step_size as f64) * PI),
                            (((end_index - internal_index) as u64) * self.segments[0].range_x)
                                as f64,
                            MISHAP_RADIUS,
                        )
                    {
                        end_index += 1;
                    }
                }

                revised_ind.push([idx - idx % step_size, idx, start_index, end_index]);
                // println!("Start and End indices of {} and {} respectively",start_index,end_index);
                // println!("{} {} {} {} {} {}",host.x,host.y,host.z,13,time,host.zone);
            }
            // if ind.len() >= 2{panic!("Finished calculating indices!");}
        }

        // --------
        // let number_of_sets:usize = NO_OF_LINES[self.zone-1];
        //EXECUTION
        vector.sort_by(|a, b| a.origin_x.cmp(&b.origin_x));
        // Create an iterator to loop over the vector in step_size chunks
        let mut chunk_iter = vector.chunks_exact_mut(step_size);

        // Initialize a counter to keep track of the global index
        let mut global_index = 0;
        let mut indices_to_infect: Vec<usize> = Vec::new();
        // if let Some(first_chunk) = chunk_iter.next() {
        //     //just an example of a wayt o access the first element per chunk
        //     if let Some(first_element) = first_chunk.first_mut(){

        //     }
        // }
        for chunk in chunk_iter {
            let mut start: Vec<usize> = Vec::new();
            let mut end: Vec<usize> = Vec::new();
            let mut trigger: Vec<usize> = Vec::new();
            let mut fail: bool = true;
            let matched_arrays: Vec<&[usize; 4]> = revised_ind
                .iter()
                // .filter(|array| MISHAP && evs[array[1]%step_size].infected && roll(MISHAP_PROBABILITY) && roll(1.0-(evs[array[1]%step_size].number_of_times_infected as f64)*EVISCERATOR_TO_HOST_PROBABILITY_DECAY) && array[0] == global_index)
                .filter(|array| array[0] == global_index)
                .collect();

            // Extract the third index from each matched array and collect into matched_indices
            for matched_array in matched_arrays {
                trigger.push(matched_array[1]);
                start.push(matched_array[2]);
                end.push(matched_array[3]);
            }

            let start: usize = match start.iter().min() {
                Some(x) => *x,
                _ => 1000,
            };
            let end: usize = match end.iter().max() {
                Some(x) => *x,
                _ => 1000,
            };

            fail = start == 1000 && end == 1000;

            for (j_ele, host) in chunk.iter_mut().enumerate() {
                let j = global_index + j_ele;
                // Compare and update the elements in the relative vector of eviscerators length
                if j_ele >= start && j_ele <= end && !fail && roll(MISHAP_TRANSFER_PROBABILITY) {
                    //THIS SECTION HERE IS WHERE THE MISHAP explosions are applied RIGHT BEFORE the eviscerators
                    host.infected = true; //This is taking it that the moment a chicken's infected guts touch another chicken's, it's been contaminated at 100% chance, so we just set to true given the prevailing conditions have been met
                    host.contaminated = true; //infected = contaminated
                    println!(
                        "{} {} {} {} {} {}",
                        host.x, host.y, host.z, 13, time, host.zone
                    );
                }
                let mut eviscerator: &mut Eviscerator = &mut evs[j % step_size];
                if (host.infected || host.contaminated)
                    && host.zone == eviscerator.zone
                    && !host.eviscerated
                    && host.transfer(CONTACT_TRANSMISSION_PROBABILITY[host.zone])
                {
                    eviscerator.infected = true;
                    // println!("EVISCERATOR HAS BEEN INFECTED AT TIME {} of this host stock entering zone!",host.time);
                    eviscerator.number_of_times_infected = 0;
                    println!(
                        "{} {} {} {} {} {}",
                        host.x, host.y, host.z, 12, time, host.zone
                    );
                } else if eviscerator.infected && host.zone == eviscerator.zone && !host.eviscerated
                {
                    // println!("Confirming that an eviscerator is infected in zone {}",eviscerator.zone);
                    host.infected = host.transfer(limits::max(
                        0.0,
                        (1.0 - (eviscerator.number_of_times_infected as f64)
                            * EVISCERATOR_TO_HOST_PROBABILITY_DECAY)
                            * CONTACT_TRANSMISSION_PROBABILITY[host.zone],
                    )) || host.infected;
                    host.contaminated = host.infected || host.contaminated; //In evisceration process, an eviscerator infecting a chicken is equivalent to contaminating as well simultaneously!
                    eviscerator.number_of_times_infected += 1;
                    if host.infected {
                        println!(
                            "{} {} {} {} {} {}",
                            host.x, host.y, host.z, 11, time, host.zone
                        );
                    }
                }
                //Decay of infection
                if eviscerator.number_of_times_infected >= EVISCERATE_DECAY {
                    eviscerator.infected = false;
                }
                // host.eviscerated = true;
                // if MISHAP && host.infected && ind.contains(&j) && MISHAP_RADIUS>minimum_distance && roll(MISHAP_PROBABILITY){
                // }
            }
            global_index += step_size;
        }
        // println!("We have {} contaminated hosts after processing",vector1.clone().into_par_iter().filter(|x| x.motile == 0 && x.contaminated).collect::<Vec<_>>().len() as u64);
        let cloned_eviscerators: Vec<Eviscerator> = evs.iter().cloned().collect();

        // Return the cloned vector
        cloned_eviscerators
    }
}

#[derive(Clone)]
pub struct host {
    eviscerated: bool,
    contaminated: bool,
    infected: bool,
    number_of_times_infected: u32,
    time_infected: f64,
    generation_time: f64,
    colonized: bool,
    motile: u8,
    zone: usize, //Possible zones denoted by ordinal number sequence
    prob1: f64, //Probability of contracting disease - these are tied to zone if you create using .new() implementation within methods
    prob2: f64, //standard deviation if required OR second probabiity value for transferring in case that is different from prob1
    x: f64,
    y: f64,
    z: f64, //can be 0 if there is no verticality
    perched: bool,
    eating: bool,
    eat_x: f64,
    eat_y: f64,
    eating_time: f64,
    age: f64,  //Age of host
    time: f64, //Time host has spent in facility - start from 0.0 from zone 0
    origin_x: u64,
    origin_y: u64,
    origin_z: u64,
    restrict: bool, //Are the hosts free roaming or not?
    range_x: u64, //"Internal" GRIDSIZE to simulate caged hosts in side the zone itself, not free roaming within facility ->Now to be taken from Segment
    range_y: u64, //Same as above but for the y direction
    range_z: u64,
}
//Note that if you want to adjust the number of zones, you have to, in addition to adjusting the individual values to your liking per zone, also need to change the slice types below!
//Resolution
const STEP: [[usize; 3]; 2] = [[4, 4, 4], [2, 2, 2]]; //Unit distance of segments ->Could be used to make homogeneous zoning (Might not be very flexible a modelling decision)
const HOUR_STEP: f64 = 4.0; //Number of times hosts move per hour
const LENGTH: usize = 20; //How long do you want the simulation to be?
                          //Infection/Colonization module
                          // ------------Do only colonized hosts spread disease or do infected hosts spread
const PERCENT_INF: f64 = 8.36 / 100.0;
const TOTAL_NO_OF_HOSTS: f64 = 21000.0;
const HOST_0: usize = (PERCENT_INF * TOTAL_NO_OF_HOSTS) as usize; //8.36% of population infected
const COLONIZATION_SPREAD_MODEL: bool = true;
const TIME_OR_CONTACT: bool = true; //true for time -> contact uses number of times infected to determine colonization
const IMMORTAL_CONTAMINATION: bool = true;
//If you want to use a truncated normal distribution for time to go from infected to colonized ...
const TIME_TO_COLONIZE: [f64; 2] = [5.0 * 24.0, 11.6 * 24.0]; //95% CI for generation time
const COLONIZE_TIME_MAX_OVERRIDE: f64 = 26.0 * 24.0;
//If you want to use a gamma distribution
const ADJUSTED_TIME_TO_COLONIZE: [f64; 2] = [4.5, 1.0 / 2.0]; //In days, unlike hours. This is converted to hours within code
const PROBABILITY_OF_HORIZONTAL_TRANSMISSION: f64 = 0.05; //Chance of infected, but not yet colonized host, infecting their own deposits-> eggs
                                                          // const RATE_OF_COLONIZATION_DECAY: f64 = 0.17;
const FECAL_SHEDDING_PERIOD: f64 = 19.77 * 24.0; //Period in of which faeces from infected hosts will be infected, after which they will not be.
const RECOVERY_RATE: [f64; 2] = [0.002, 0.008]; //Lower and upper range that increases with age

const NO_TO_COLONIZE: u32 = 100;
//Contamination rules | Different from infections, which are taken to only be transmittable via feeding of infected faeces
const HOSTTOHOST_CONTACT_SPREAD: bool = false; // Host -> Host, Host -> Faeces and Host -> Eggs via spatial proximity
const HOSTTOEGG_CONTACT_SPREAD: bool = true;
const HOSTTOFAECES_CONTACT_SPREAD: bool = false;
const EGGTOHOST_CONTACT_SPREAD: bool = true;
const FAECESTOHOST_CONTACT_SPREAD: bool = true;
const EGGTOFAECES_CONTACT_SPREAD: bool = true;
const FAECESTOEGG_CONTACT_SPREAD: bool = true;
// const INITIAL_COLONIZATION_RATE:f64 = 0.47; //Probability of infection, resulting in colonization -> DAILY RATE ie PER DAY
//Space
const LISTOFPROBABILITIES: [f64; 2] = [0.9; 2]; //Probability of transfer of disease per zone - starting from zone 0 onwards
const CONTACT_TRANSMISSION_PROBABILITY: [f64; 2] = [0.205; 2];
const GRIDSIZE: [[f64; 3]; 2] = [[4.0 * TOTAL_NO_OF_HOSTS, 4.0, 4.0], [28000.0, 2.0, 2.0]];
const MAX_MOVE: f64 = 10.0;
const MEAN_MOVE: f64 = 4.0;
const STD_MOVE: f64 = 3.0; // separate movements for Z config
const MAX_MOVE_Z: f64 = 1.0;
const MEAN_MOVE_Z: f64 = 2.0;
const STD_MOVE_Z: f64 = 4.0;
const NO_OF_HOSTS_PER_SEGMENT: [u64; 2] = [1, 1];
//Anchor points
//Vertical perches
const PERCH: bool = false;
const PERCH_ZONES: [usize; 1] = [4];
const PERCH_HEIGHT: f64 = 2.0; //Number to be smaller than segment range z -> Denotes frequency of heights at which hens can perch
const PERCH_FREQ: f64 = 0.5; //probability that hosts go to perch
const DEPERCH_FREQ: f64 = 0.4; //probability that a host when already on perch, decides to go down from perch
                               //Nesting areas
const NEST: bool = false;
const NESTING_AREA: f64 = 0.25; //ratio of the total area of segment in of which nesting area is designated - min x y z side
                                //Space --- Segment ID
const TRANSFERS_ONLY_WITHIN: [bool; 2] = [true, false]; //Boolean that informs simulation to only allow transmissions to occur WITHIN segments, not between adjacent segments
                                                        //Fly option
const FLY: bool = false;
const FLY_FREQ: u8 = 3; //At which Hour step do the
                        //Disease
const TRANSFER_DISTANCE: f64 = 1.0; //maximum distance over which hosts can trasmit diseases to one another
const CONTAMINATE_TRANSFER_DISTANCE: f64 = 1.0;
const SIZE_FACTOR_FOR_EGGS: f64 = 0.15; //eggs are significantly smaller than their original hosts, so it stands to reason that their transfer distance for contact spread should be smaller
                                        //Host parameters
const MEAN_AGE: f64 = 17.0 * 7.0 * 24.0; //Mean age of hosts imported (IN HOURS)
const STD_AGE: f64 = 3.0 * 24.0; //Standard deviation of host age (when using normal distribution)
const MAX_AGE: f64 = 20.0 * 7.0 * 24.0; //Maximum age of host accepted (Note: as of now, minimum age is 0.0)
const DEFECATION_RATE: f64 = 6.0; //Number times a day host is expected to defecate
const MIN_AGE: f64 = 1.0 * 24.0;

const DEPOSIT: bool = false;
const DEPOSIT_RATE_AFFECTED_BY_INFECTION: bool = true;
const DEPOSIT_RATE: f64 = 6.0 / 7.0; //Number of times a day host is expected to deposit a consumable deposit
const DEPOSIT_RATE_INFECTION_MULTIPLIER: f64 = 2.0 / 3.0;
//

//Feed parameters
const FEED_1: bool = false; //Do the hosts get fed - omnipotent method -> Like feeder belts (adjust Feed infected and Feed infection rate for that) or simply one on one at the same time (true omnipotent method)
const FEED_2: bool = false; //Do the hosts get fed - with standalone feeders ->crowding implication
const FEED_INFECTED: f64 = 0.11; //Proportion of times that feed gets infected - set to 1.0 if you simply want to simulate separate feed sources that are independent of each other in terms of being infected at start
const FEED_INFECTION_RATE: f64 = 0.8; //Probability of INFECTED FEED infecting hosts that consume it - CAN either mean a. probability that independent feed is infected (ind. of other feed sources being infected at the time) b. ALL feed at time is infected, and this denotes chance that consumption of infected feed leads to infection in host
const FEED_ZONES: [usize; 3] = [0, 1, 2]; //To set the zones that have feed provided to them.
const FEED_TIMES: [usize; 2] = [5, 11]; //24h format, when hosts get fed: Does not have to be only 2 - has no link to number of zones or anything like that
const FEEDER_SPACING: f64 = 2.5;
const FEED_DURATION: f64 = 0.5;

//Purge/Slaughter parameters
const SLAUGHTER_POINT: usize = 0; //Somewhere in zone {}, the hosts are slaughtered/killed and will cease to produce any eggs or faeces
                                  //Evisceration parameters
                                  //We assume that all hosts are isolated from the rest of the hosts in the zone per evisceration "unit"
const EVISCERATE: bool = true;
const EVISCERATE_ZONES: [usize; 1] = [1]; //Zone in which evisceration takes place
const EVISCERATE_DECAY: u8 = 5;
const NO_OF_PROBES: [usize; 1] = [28; 1]; //no of probes per eviscerator
const LINE_NO: usize = 1;
const NO_OF_LINES: [usize; 1] = [LINE_NO; 1];
const NO_OF_EVISCERATORS: usize = 3;
const EVISCERATOR_TO_HOST_PROBABILITY_DECAY: f64 = 0.25; //Multiplicative decrease of  probability - starting from LISTOFPROBABILITIES value 100%->75% (if 0.25 is value)->50% ->25%->0%
const CLEAN_EVISCERATORS: bool = false; //Be sure to set the hours when eviscerators are manually cleaned yourself (Might need to run simulation to figure out when evisceraors get  used at all)
const SERIAL_EVISCERATION: bool = false; //Are the hosts going through multiple eviscerations? (ideally via consecutive zones or through a separately implemented logic)

const CURVATURE: bool = true;
//We are assuming that when eviscerators are brought into a circle, the distance between them is maintained - inevitably determining the radius of the curvature
//In other words, we are assuming that all the eviscerator probes fit within a semicircle, and that there is no extension of this range of eviscerators
const PI: f64 = std::f64::consts::PI;
const ANGLE_MAXIMA: f64 = 0.25 * PI; //Maximum angular displacement, above which, mishaps cannot travel anyway
                                     //Evisceration -------------> Mishap/Explosion parameters
const MISHAP: bool = true;
const MISHAP_PROBABILITY: f64 = 0.1338;
const MISHAP_TRANSFER_PROBABILITY: f64 = 0.4;
const MISHAP_RADIUS: f64 = 2.0; //Must be larger than the range_x of the eviscerate boxes for there to be any change in operation
                                //Transfer parameters
const ages: [f64; 2] = [0.01, 1.0]; //Time hosts are expected spend in each region minimally
                                    //Collection
const AGE_OF_HOSTCOLLECTION: f64 = 20.0 * 24.0; //For instance if you were collecting hosts every 15 days
const COLLECT_DEPOSITS: bool = true;
const AGE_OF_DEPOSITCOLLECTION: f64 = 1.0 * 24.0; //If you were collecting their eggs every 3 days
const FAECAL_CLEANUP_FREQUENCY: usize = 2; //How many times a day do you want faecal matter to be cleaned up?
                                           //or do we do time collection instead?
                                           //Influx? Do you want new hosts being fed into the scenario everytime the first zone exports some to the succeeding zones?
const INFLUX: bool = false;
const PERIOD_OF_INFLUX: u8 = 2; //How many hours before new batch of hosts are imported?
const PERIOD_OF_TRANSPORT: u8 = 1;
const PROBABILITY_OF_INFECTION: f64 = 0.01; //probability of imported host being infected
                                            //Restriction?
const RESTRICTION: bool = true;
//Generation Parameters
const SPORADICITY: f64 = 1000.0; //How many fractions of the dimension of the cage/segment do you want the hosts to start at? Bigger number makes the spread of hosts LESS : starting points end up being centered

//Additional 3D parameters
const FAECAL_DROP: bool = false; //Does faeces potentially drop in terms of depth?
const PROBABILITY_OF_FAECAL_DROP: f64 = 0.3;

impl host {
    fn recover(mut vector: &mut Vec<host>) {
        vector.iter_mut().for_each(|mut x| {
            if x.infected && x.motile == 0 {
                let grad: f64 = (RECOVERY_RATE[1] - RECOVERY_RATE[0]) / (MAX_AGE - MIN_AGE);
                let prob: f64 = RECOVERY_RATE[0] + (x.age - MIN_AGE) * grad;
                if roll(prob) {
                    if !IMMORTAL_CONTAMINATION {
                        x.contaminated = false;
                    }
                    x.infected = false;
                    x.colonized = false;
                    x.time_infected = 0.0;
                    x.number_of_times_infected = 0;
                    x.generation_time =
                        gamma(ADJUSTED_TIME_TO_COLONIZE[0], ADJUSTED_TIME_TO_COLONIZE[1]) * 24.0;
                }
            }
        })
    }
    fn feed(
        mut vector: &mut Vec<host>,
        origin_x: u64,
        origin_y: u64,
        origin_z: u64,
        zone: usize,
        time: usize,
    ) {
        if FEED_1 && roll(FEED_INFECTED) {
            // println!("Infected feed confirmed");
            vector.iter_mut().for_each(|mut h| {
                if roll(FEED_INFECTION_RATE)
                    && h.motile == 0
                    && !h.infected
                    && h.origin_x == origin_x
                    && h.origin_y == origin_y
                    && h.origin_z == origin_z
                    && h.zone == zone
                {
                    h.infected = h.transfer(1.0);
                    println!("{} {} {} {} {} {}", h.x, h.y, h.z, 10, time, h.zone);
                    //10 is now an interaction type driven by the infected feed
                }
            })
        } else if FEED_2 {
            //Identify locations of feeders based off of spacing provided
            // println!("HERE");
            let [range_x, range_y, _] = STEP[zone];
            let x_no = limits::max(range_x as f64 / FEEDER_SPACING, 2.0) as usize;
            let y_no = limits::max(2.0, range_y as f64 / FEEDER_SPACING) as usize;
            let no: u64 = (x_no as u64 - 1) * (y_no as u64 - 1);
            vector.iter_mut().for_each(|mut h| {
                if h.motile == 0
                    && h.origin_x == origin_x
                    && h.origin_y == origin_y
                    && h.origin_z == origin_z
                    && h.zone == zone
                {
                    h.eating = true;
                }
            });
            let total_to_feed: Vec<host> = vector
                .iter()
                .filter(|&h| {
                    h.motile == 0
                        && h.origin_x == origin_x
                        && h.origin_y == origin_y
                        && h.origin_z == origin_z
                        && h.zone == zone
                })
                .cloned()
                .collect();
            let total_to_feed: usize = total_to_feed.len();
            let per: usize = total_to_feed / (no as usize);
            // let counter:usize = 0;
            for x in 1..x_no {
                //indices of the feeder -> not location
                for y in 1..y_no {
                    let is_feed_infected: bool = roll(FEED_INFECTED); //IS THE FEED AT THIS LOCATION INFECTED?
                                                                      //relative origin_location to segment frame of reference
                    let x_location = FEEDER_SPACING * (x as f64);
                    let y_location = FEEDER_SPACING * (y as f64);
                    // Determine how many elements to modify for the current iteration
                    let start_index: usize = (x - 1) * y_no + (y - 1);
                    // let end_index = start_index + per;
                    vector
                        .iter_mut()
                        .filter(|h| {
                            h.motile == 0
                                && h.origin_x == origin_x
                                && h.origin_y == origin_y
                                && h.origin_z == origin_z
                                && h.zone == zone
                        })
                        .skip(start_index)
                        .take(per)
                        .for_each(|h| {
                            h.eat_x = x_location;
                            h.eat_y = y_location;
                            if is_feed_infected && !h.infected && roll(FEED_INFECTION_RATE) {
                                h.infected = h.transfer(1.0);
                                println!("{} {} {} {} {} {}", h.x, h.y, h.z, 10, time, h.zone);
                                //10 is now an interaction type driven by the infected feed
                            }
                        })
                }
            }
            // if roll(FEED_INFECTED){
            //     vector.iter_mut().for_each(|mut h|{
            //         if roll(FEED_INFECTION_RATE) && h.motile == 0 && !h.infected && h.origin_x == origin_x && h.origin_y == origin_y && h.origin_z == origin_z && h.zone == zone{
            //             h.infected = h.transfer(1.0);
            //             println!("{} {} {} {} {} {}",h.x,h.y,h.z,10,time,h.zone); //10 is now an interaction type driven by the infected feed
            //         }
            //     })
            // }
        }
    }
    fn infect(mut vector: Vec<host>, loc_x: u64, loc_y: u64, loc_z: u64, zone: usize) -> Vec<host> {
        if let Some(first_host) = vector
            .iter_mut()
            .filter(|host_| host_.zone == zone)
            .min_by_key(|host| {
                let dx = host.origin_x as i64 - loc_x as i64;
                let dy = host.origin_y as i64 - loc_y as i64;
                let dz = host.origin_z as i64 - loc_z as i64;
                (dx * dx + dy * dy + dz * dz) as u64
            })
        {
            if !first_host.infected {
                first_host.infected = true;
            }
        }
        vector
    }
    fn infect_multiple(
        mut vector: Vec<host>,
        loc_x: u64,
        loc_y: u64,
        loc_z: u64,
        n: usize,
        zone: usize,
        colonized: bool,
    ) -> Vec<host> {
        //homogeneous application ->Periodically apply across space provided,->Once per location
        let mut filtered_vector: Vec<&mut host> = vector
            .iter_mut()
            .filter(|host| host.zone == zone && !host.infected)
            .collect();

        filtered_vector.sort_by_key(|host| {
            let dx = host.origin_x as i64 - loc_x as i64;
            let dy = host.origin_y as i64 - loc_y as i64;
            let dz = host.origin_z as i64 - loc_z as i64;
            (dx * dx + dy * dy + dz * dz) as u64
        });
        for host in filtered_vector.iter_mut().take(n) {
            host.infected = true;
            host.contaminated = true; //turn this off if you don't want to consider an infected host also a physically contaminated host.
            if colonized {
                host.colonized = true;
            }
            println!(
                "{} {} {} {} {} {}",
                host.x, host.y, host.z, 0, 0.0, host.zone
            );
        }
        vector
    }
    fn contaminate_multiple(
        mut vector: Vec<host>,
        loc_x: u64,
        loc_y: u64,
        loc_z: u64,
        n: usize,
        zone: usize,
    ) -> Vec<host> {
        //homogeneous application ->Periodically apply across space provided,->Once per location
        let mut filtered_vector: Vec<&mut host> =
            vector.iter_mut().filter(|host| host.zone == zone).collect();

        filtered_vector.sort_by_key(|host| {
            let dx = host.origin_x as i64 - loc_x as i64;
            let dy = host.origin_y as i64 - loc_y as i64;
            let dz = host.origin_z as i64 - loc_z as i64;
            (dx * dx + dy * dy + dz * dz) as u64
        });
        for host in filtered_vector.iter_mut().take(n) {
            host.contaminated = true;
            // if colonized{host.colonized = true;}
            println!(
                "{} {} {} {} {} {}",
                host.x, host.y, host.z, 0, 0.0, host.zone
            );
        }
        vector
    }
    fn transport(mut vector: &mut Vec<host>, space: &mut Vec<Zone_3D>, influx: bool) {
        //Also to change ;size if you change number of zones
        let mut output: Vec<host> = Vec::new();
        // println!("Influx is set to {}", influx);
        // println!("Capacity for zone 0 is {}",space[0].capacity);
        for zone in (0..space.len()).rev() {
            // let mut __:u32 = space.clone()[zone].capacity;
            if &space[zone].capacity > &0 && zone > 0 {
                //If succeeding zones (obviously zone 0 doesn't have do to this - that needs to be done with a replace mechanism)
                // let zone_toedit:&mut Zone_3D = &mut space[zone];
                //RANDOMIZE BEFORE PICKING FOR TRANSPORT TO ALLOW FOR RANDOM EFFECTS
                // vector.as_mut_slice().shuffle(&mut rand::thread_rng());
                vector.sort_by_key(|s| (s.x as i32, s.y as i32, s.z as i32));
                vector.iter_mut().for_each(|mut x| {
                    if x.zone == zone - 1
                        && x.time > ages[zone - 1]
                        && x.motile == 0
                        && space[zone].capacity > 0
                        && ((!EVISCERATE || EVISCERATE_ZONES.contains(&x.zone) == false)
                            || EVISCERATE && EVISCERATE_ZONES.contains(&x.zone) && x.eviscerated)
                    {
                        //Hosts in previous zone that have spent enough time spent in previous zone
                        //Find the first available segment
                        // println!("Transporting...");
                        // println!("Current szone")
                        // if x.infected && x.zone ==0{println!("Infected");}
                        // else if !x.infected && x.zone == 0{println!("Not infected");}
                        // __ -= 1;
                        // println!("Capacity for zone {} is now:{} - pre subtraction",zone-1,space[zone-1].capacity);
                        space[zone - 1].subtract(
                            x.origin_x.clone(),
                            x.origin_y.clone(),
                            x.origin_z.clone(),
                        ); //move host from previous zone
                           // println!("Capacity for zone {} is now:{} - post subtraction",zone-1,space[zone-1].capacity);
                           // println!("---------------------------------------");
                           // println!("{} capacity for zone {} vs {} for zone {}", &space[zone-1].capacity, zone-1,&space[zone].capacity,zone);
                        x.zone += 1;
                        // println!("Moved to zone {}",x.zone);
                        x.time = 0.0;
                        x.prob1 = LISTOFPROBABILITIES[x.zone];
                        x.prob2 = CONTACT_TRANSMISSION_PROBABILITY[x.zone];
                        // println!("Going to deduct capacity @  zone {} with a capacity of {}", zone,zone_toedit.clone().zone);
                        //SERIAL EVISCERATION LOGIC - if the host is to get eviscerated AGAIN in another zone, SERIAL EVISCERATION needs to beset to true to RESET the eviscerated status
                        if SERIAL_EVISCERATION && x.eviscerated {
                            x.eviscerated = false;
                        }

                        // println!("Apparently think that zone {} has {} space left",zone,space[zone].capacity);
                        // println!("Capacity for zone {} is now:{} - pre addition",zone,space[zone].capacity);
                        let vars: [u64; 7] = space[zone].add();
                        // println!("Capacity for zone {} is now:{} - pot addition",zone,space[zone].capacity);
                        if vars[0] != 0 {
                            x.origin_x = vars[1];
                            x.origin_y = vars[2];
                            x.origin_z = vars[3];
                            x.range_x = vars[4];
                            x.range_y = vars[5];
                            x.range_z = vars[6];

                            //Maybe try moving the hosts randomly within each new section otherwise they all will infect each other at origin
                            let mean_x: f64 =
                                (x.origin_x as f64) + ((x.range_x as f64) / 2.0) as f64;
                            let std_x: f64 = ((x.range_x as f64) / SPORADICITY) as f64;
                            // let max_x:f64 = x.range_x as f64;
                            let mean_y: f64 =
                                (x.origin_y as f64) + ((x.range_y as f64) / 2.0) as f64;
                            let std_y: f64 = ((x.range_y as f64) / SPORADICITY) as f64;
                            // let max_y:f64 = x.range_y as f64;
                            //Baseline starting point in new region
                            x.x = normal(mean_x, std_x, (x.origin_x + x.range_x) as f64);
                            x.y = normal(mean_y, std_x, (x.origin_y + x.range_y) as f64);
                            x.z = x.origin_z as f64;
                        }
                    }
                })
            // output.append(&mut vector);
            } else if zone == 0 && space[zone].capacity > 0 && influx {
                //replace mechanism : influx is determined by INFLUX and PERIOD OF INLFUX
                // let mut zone_0:&mut Zone = &mut space[0];
                // println!("Importing");
                for _ in 0..space[zone].clone().capacity as usize {
                    // let [x,y]:[u64;2] = space[zone].add();
                    //Roll probability
                    let mut rng = thread_rng();
                    let roll = Uniform::new(0.0, 1.0);
                    let rollnumber: f64 = rng.sample(roll);
                    let [condition, x, y, z, range_x, range_y, range_z] = space[0].add();
                    if rollnumber < PROBABILITY_OF_INFECTION && condition != 0 {
                        vector.push(host::new_inf(
                            0,
                            CONTACT_TRANSMISSION_PROBABILITY[zone],
                            x as f64,
                            y as f64,
                            z as f64,
                            RESTRICTION,
                            range_x,
                            range_y,
                            range_z,
                        ));
                    } else if condition != 0 {
                        vector.push(host::new(
                            0,
                            CONTACT_TRANSMISSION_PROBABILITY[zone],
                            x as f64,
                            y as f64,
                            z as f64,
                            RESTRICTION,
                            range_x,
                            range_y,
                            range_z,
                        ));
                    }
                }
            }
        }
    }

    fn transfer(&self, mult: f64) -> bool {
        //using prob1 as the probability of contracting disease  (in other words, no separation of events between transferring and capturing disease. If something is infected, it is always infected. Potentially.... the prospective new host will not get infected, but the INFECTED is always viably transferring)
        let mut rng = thread_rng();
        let roll = Uniform::new(0.0, 1.0);
        let rollnumber: f64 = rng.sample(roll);
        // println!("DISEASE   {}",rollnumber);
        rollnumber < self.prob1 * mult
    }
    fn new(
        zone: usize,
        std: f64,
        loc_x: f64,
        loc_y: f64,
        loc_z: f64,
        restriction: bool,
        range_x: u64,
        range_y: u64,
        range_z: u64,
    ) -> host {
        //We shall make it such that the host is spawned within the bottom left corner of each "restricted grid" - ie cage
        let prob: f64 = LISTOFPROBABILITIES[zone.clone()];
        //Add a random age generator
        host {
            eviscerated: false,
            contaminated: false,
            infected: false,
            number_of_times_infected: 0,
            time_infected: 0.0,
            generation_time: gamma(ADJUSTED_TIME_TO_COLONIZE[0], ADJUSTED_TIME_TO_COLONIZE[1])
                * 24.0,
            colonized: false,
            motile: 0,
            zone: zone,
            prob1: prob,
            prob2: std,
            x: loc_x as f64,
            y: loc_y as f64,
            z: loc_z as f64,
            perched: false,
            eating: false,
            eat_x: 0.0,
            eat_y: 0.0,
            eating_time: 0.0,
            age: normal_(MIN_AGE, MEAN_AGE, STD_AGE, MAX_AGE),
            time: 0.0,
            origin_x: loc_x as u64,
            origin_y: loc_y as u64,
            origin_z: loc_z as u64,
            restrict: restriction,
            range_x: range_x,
            range_y: range_y,
            range_z: range_z,
        }
    }
    fn new_inf(
        zone: usize,
        std: f64,
        loc_x: f64,
        loc_y: f64,
        loc_z: f64,
        restriction: bool,
        range_x: u64,
        range_y: u64,
        range_z: u64,
    ) -> host {
        //presumably a newly infected host that spreads disease is colonized
        let prob: f64 = LISTOFPROBABILITIES[zone.clone()];
        host {
            eviscerated: false,
            contaminated: false,
            infected: true,
            number_of_times_infected: 0,
            time_infected: 0.0,
            generation_time: gamma(ADJUSTED_TIME_TO_COLONIZE[0], ADJUSTED_TIME_TO_COLONIZE[1])
                * 24.0,
            colonized: true,
            motile: 0,
            zone: zone,
            prob1: prob,
            prob2: std,
            x: loc_x as f64,
            y: loc_y as f64,
            z: loc_z as f64,
            perched: false,
            eating: false,
            eat_x: 0.0,
            eat_y: 0.0,
            eating_time: 0.0,
            age: normal_(MIN_AGE, MEAN_AGE, STD_AGE, MAX_AGE),
            time: 0.0,
            origin_x: loc_x as u64,
            origin_y: loc_y as u64,
            origin_z: loc_z as u64,
            restrict: restriction,
            range_x: range_x,
            range_y: range_y,
            range_z: range_z,
        }
    }
    fn deposit(&mut self, consumable: bool) -> host {
        //Direct way to lay deposit from host. The function is 100% deterministic and layering a probability clause before this is typically expected
        let zone = self.zone.clone();
        let prob1: f64 = self.prob1.clone();
        let prob2: f64 = self.prob2.clone();
        let mut x: f64 = self.x.clone();
        let mut y: f64 = self.y.clone();
        let mut z = self.origin_z.clone() as f64;
        if !RESTRICTION {
            //If there are no containers holding the hosts (ie RESTRICTION), these hosts are keeping themselves above z = 0 by flying/floating etc, then deposits will necessary FALL to the floor ie z = 0
            z = 0.0;
        }
        // println!("Probability of infected drop now is {}",PROBABILITY_OF_HORIZONTAL_TRANSMISSION*(1.0-RATE_OF_COLONIZATION_DECAY).powf(self.time_infected));
        let mut inf = false;
        if !COLONIZATION_SPREAD_MODEL {
            inf = self.infected;
        } else if self.colonized {
            inf = true;
        } else if self.infected && self.time_infected < FECAL_SHEDDING_PERIOD && !consumable {
            inf = true;
        } else if self.infected && consumable {
            inf = roll(PROBABILITY_OF_HORIZONTAL_TRANSMISSION);
        }
        let range_y = self.range_y.clone();
        let range_x = self.range_x.clone();
        let range_z = self.range_z.clone();
        let restriction = self.restrict.clone();
        let origin_x = self.origin_x.clone();
        let origin_y = self.origin_y.clone();
        let origin_z = self.origin_z.clone();

        // println!("EGG BEING LAID");
        //Logic: If infected, immediately count as colonized for egg and faeces. Don't need to wait for it to be considered an infectant whichever colonization or non colonization model we use
        if consumable {
            //Nesting logic application
            if NEST {
                //egg location
                x = uniform(
                    origin_x as f64,
                    origin_x as f64 + NESTING_AREA * (range_x as f64),
                );
                y = uniform(
                    origin_y as f64,
                    origin_y as f64 + NESTING_AREA * (range_y as f64),
                );
                //host location
                self.perched = false;
                self.x = x.clone();
                self.y = y.clone();
            }
            host {
                eviscerated: false,
                contaminated: inf,
                infected: inf,
                number_of_times_infected: 0,
                time_infected: 0.0,
                generation_time: self.generation_time,
                colonized: inf,
                motile: 1,
                zone: zone,
                prob1: prob1,
                prob2: prob2,
                x: x,
                y: y,
                z: z,
                perched: false,
                eating: self.eating,
                eat_x: self.eat_x,
                eat_y: self.eat_y,
                eating_time: self.eating_time,
                age: 0.0,
                time: 0.0,
                origin_x: x as u64,
                origin_y: y as u64,
                origin_z: z as u64,
                restrict: restriction,
                range_x: range_x,
                range_y: range_y,
                range_z: range_z,
            }
            //Returning new egg host to host vector
        } else {
            //fecal shedding

            // println!("Pooping!");
            host {
                eviscerated: false,
                contaminated: inf,
                infected: inf,
                number_of_times_infected: 0,
                time_infected: 0.0,
                generation_time: self.generation_time,
                colonized: inf,
                motile: 2,
                zone: zone,
                prob1: prob1,
                prob2: prob2,
                x: x,
                y: y,
                z: z,
                perched: false,
                eating: self.eating,
                eat_x: self.eat_x,
                eat_y: self.eat_y,
                eating_time: self.eating_time,
                age: 0.0,
                time: 0.0,
                origin_x: x as u64,
                origin_y: y as u64,
                origin_z: z as u64,
                restrict: restriction,
                range_x: range_x,
                range_y: range_y,
                range_z: range_z,
            }
        }
    }
    fn deposit_all(vector: Vec<host>) -> Vec<host> {
        //Below is an example whereby hosts deposit twice a day (fecal matter and laying eggs each once per day as an example)
        let mut vecc: Vec<host> = vector.clone();
        let mut vecc_into: Vec<host> = vector
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 0 && x.zone < SLAUGHTER_POINT)
            .collect::<Vec<_>>(); //With this re are RETAINING the hosts and deposits within the original vector

        //.map wasn't working so we brute forced a loop
        for ele in vecc_into {
            if DEPOSIT {
                let mut rng = thread_rng();
                let mut deposit_rate: f64 = DEPOSIT_RATE;
                if DEPOSIT_RATE_AFFECTED_BY_INFECTION && ele.infected {
                    deposit_rate *= DEPOSIT_RATE_INFECTION_MULTIPLIER;
                }
                let v: &Vec<f64> = &Poisson::new(deposit_rate / 24.0)
                    .unwrap()
                    .sample_iter(&mut rng)
                    .take(1)
                    .collect();
                for _ in 0..v[0] as usize {
                    vecc.push(ele.clone().deposit(true)); //non consumable excrement once per day rate
                }
            }

            let mut rng = thread_rng();
            let v: &Vec<f64> = &Poisson::new(DEFECATION_RATE / 24.0)
                .unwrap()
                .sample_iter(&mut rng)
                .take(1)
                .collect();
            for _ in 0..v[0] as usize {
                vecc.push(ele.clone().deposit(false)); //non consumable excrement once per day rate
            }
        }
        vecc
    }
    fn land(vector: Vec<host>) -> Vec<host> {
        vector
            .into_par_iter()
            .filter_map(|mut x| {
                if RESTRICTION {
                    x.z = x.origin_z as f64;
                    Some(x)
                } else {
                    x.z = 0.0;
                    Some(x)
                }
            })
            .collect()
    }
    fn shuffle(mut self) -> host {
        let mut eating_time: f64 = self.eating_time.clone();
        let mut eating: bool = self.eating.clone();
        if self.eating {
            if self.eating_time < FEED_DURATION {
                eating_time += 1.0 / HOUR_STEP;
            } else {
                eating_time = 0.0;
                eating = false;
            }
        }
        if self.infected {
            self.time_infected += 1.0 / HOUR_STEP;
        }
        // let initial_colonization_rate_h:f64 = 1.0-(1.0-INITIAL_COLONIZATION_RATE).powf(1.0/(24.0*HOUR_STEP));
        if (TIME_OR_CONTACT && !self.colonized && self.infected)
            && COLONIZATION_SPREAD_MODEL
            && self.motile == 0
        {
            if self.time_infected > self.generation_time {
                self.colonized = true;
            }
        } else if COLONIZATION_SPREAD_MODEL
            && !TIME_OR_CONTACT
            && self.number_of_times_infected > NO_TO_COLONIZE
            && self.infected
            && self.motile == 0
        {
            self.colonized = true;
        }
        if self.motile == 0
            && (!EVISCERATE || EVISCERATE_ZONES.contains(&self.zone) == false)
            && !self.eviscerated
        {
            // NOT IN EVISCERATION
            //Whether the movement is negative or positive
            let mut mult: [f64; 3] = [0.0, 0.0, 0.0];
            for index in 0..mult.len() {
                if roll(0.33) {
                    if roll(0.5) {
                        mult[index] = 1.0;
                    } else {
                        mult[index] = -1.0;
                    }
                }
            }

            let mut new_x: f64 = self.x.clone() as f64;
            let mut new_y: f64 = self.y.clone() as f64;
            let mut new_z: f64 = self.z.clone() as f64;
            //use truncated normal distribution (which has been forced to be normal) in order to change the values of x and y accordingly of the host - ie movement
            if self.restrict {
                // println!("We are in the restrict clause! {}", self.motile);
                // println!("Current shuffling parameter is {}", self.motile);
                if !self.eating {
                    new_x = limits::min(
                        limits::max(
                            self.origin_x as f64,
                            self.x + mult[0] * normal(MEAN_MOVE, STD_MOVE, MAX_MOVE),
                        ),
                        self.origin_x as f64 + self.range_x as f64,
                    );
                } else {
                    // conservative movement pattern of the 2 chosen for the normal distribution when the chickens have gathered at the various nodes to feed.
                    self.perched = false;
                    new_x = limits::min(
                        limits::max(
                            self.origin_x as f64,
                            self.eat_x
                                + mult[0]
                                    * normal(
                                        limits::min(MEAN_MOVE, FEEDER_SPACING / 2.0),
                                        limits::min(FEEDER_SPACING / 2.0, STD_MOVE),
                                        FEEDER_SPACING,
                                    ),
                        ),
                        self.origin_x as f64 + self.range_x as f64,
                    );
                }
                if !self.perched {
                    if !self.eating {
                        new_y = limits::min(
                            limits::max(
                                self.origin_y as f64,
                                self.y + mult[1] * normal(MEAN_MOVE, STD_MOVE, MAX_MOVE),
                            ),
                            self.origin_y as f64 + self.range_y as f64,
                        );
                    } else {
                        new_y = limits::min(
                            limits::max(
                                self.origin_y as f64,
                                self.eat_y
                                    + mult[1]
                                        * normal(
                                            limits::min(MEAN_MOVE, FEEDER_SPACING / 2.0),
                                            limits::min(FEEDER_SPACING / 2.0, STD_MOVE),
                                            FEEDER_SPACING,
                                        ),
                            ),
                            self.origin_y as f64 + self.range_y as f64,
                        );
                    }
                }
                if FLY {
                    new_z = limits::min(
                        limits::max(
                            self.origin_z as f64,
                            self.z + mult[2] * normal(MEAN_MOVE_Z, STD_MOVE_Z, MAX_MOVE_Z),
                        ),
                        self.origin_z as f64 + self.range_z as f64,
                    );
                } else if PERCH && PERCH_ZONES.contains(&self.zone) && roll(PERCH_FREQ) {
                    //no need perching concept for flying creatures
                    new_z = limits::min(
                        self.z + PERCH_HEIGHT,
                        self.origin_z as f64 + self.range_z as f64,
                    );
                    self.perched = true;
                } else if PERCH
                    && PERCH_ZONES.contains(&self.zone)
                    && self.perched
                    && roll(DEPERCH_FREQ)
                {
                    new_z = self.origin_z as f64;
                    self.perched = false;
                }
            } else {
                if !self.eating {
                    new_x = limits::min(
                        limits::max(
                            0.0,
                            self.x + mult[0] * normal(MEAN_MOVE, STD_MOVE, MAX_MOVE),
                        ),
                        GRIDSIZE[self.zone as usize][0],
                    );
                } else {
                    self.perched = false;
                    new_x = limits::min(
                        limits::max(
                            0.0,
                            self.eat_x
                                + mult[0]
                                    * normal(
                                        limits::min(MEAN_MOVE, FEEDER_SPACING / 2.0),
                                        limits::min(FEEDER_SPACING / 2.0, STD_MOVE),
                                        FEEDER_SPACING,
                                    ),
                        ),
                        GRIDSIZE[self.zone as usize][0],
                    );
                }
                if !self.perched {
                    if !self.eating {
                        new_y = limits::min(
                            limits::max(
                                0.0,
                                self.y + mult[1] * normal(MEAN_MOVE, STD_MOVE, MAX_MOVE),
                            ),
                            GRIDSIZE[self.zone as usize][1],
                        );
                    } else {
                        new_y = limits::min(
                            limits::max(
                                0.0,
                                self.eat_y
                                    + mult[1]
                                        * normal(
                                            limits::min(MEAN_MOVE, FEEDER_SPACING / 2.0),
                                            limits::min(FEEDER_SPACING / 2.0, STD_MOVE),
                                            FEEDER_SPACING,
                                        ),
                            ),
                            GRIDSIZE[self.zone as usize][1],
                        );
                    }
                }
                if FLY {
                    new_z = limits::min(
                        limits::max(
                            0.0,
                            self.z + mult[2] * normal(MEAN_MOVE_Z, STD_MOVE_Z, MAX_MOVE_Z),
                        ),
                        GRIDSIZE[self.zone as usize][2],
                    );
                } else if PERCH && PERCH_ZONES.contains(&self.zone) && roll(PERCH_FREQ) {
                    //no need perching concept for flying creatures
                    new_z = limits::min(
                        self.z + PERCH_HEIGHT,
                        self.origin_z as f64 + self.range_z as f64,
                    );
                    self.perched = true;
                } else if PERCH
                    && PERCH_ZONES.contains(&self.zone)
                    && self.perched
                    && roll(DEPERCH_FREQ)
                {
                    new_z = self.origin_z as f64;
                    self.perched = false;
                }
            }
            host {
                eviscerated: self.eviscerated,
                contaminated: self.contaminated,
                infected: self.infected,
                number_of_times_infected: 0,
                time_infected: self.time_infected,
                generation_time: self.generation_time,
                colonized: self.colonized,
                motile: self.motile,
                zone: self.zone,
                prob1: self.prob1,
                prob2: self.prob2,
                x: new_x,
                y: new_y,
                z: self.z,
                perched: self.perched,
                eating: eating,
                eat_x: self.eat_x,
                eat_y: self.eat_y,
                eating_time: eating_time,
                age: self.age + 1.0 / HOUR_STEP,
                time: self.time + 1.0 / HOUR_STEP,
                origin_x: self.origin_x,
                origin_y: self.origin_y,
                origin_z: self.origin_z,
                restrict: self.restrict,
                range_x: self.range_x,
                range_y: self.range_y,
                range_z: self.range_z,
            }
        } else if self.motile == 0 && EVISCERATE && EVISCERATE_ZONES.contains(&self.zone) {
            // println!("Evisceration pending...");
            // self.motile == 1; //It should be presumably electrocuted and hung on a conveyer belt
            self.x = ((self.origin_x as f64) + (self.range_x as f64)) / 2.0; // square in middle
            self.y = ((self.origin_y as f64) + (self.range_y as f64)) / 2.0;
            self.z = (self.origin_z as f64) + (self.range_z as f64); //Place host on the top of the box to simulate suspension on the top
                                                                     // println!("Placing host @ {},{},{}",self.x,self.y,self.z);.shuffle
            self.age += 1.0 / HOUR_STEP;
            self.time += 1.0 / HOUR_STEP;
            self
        } else if self.restrict {
            //deposits by hosts do not move obviously, but they DO age, which affects collection
            self.age += 1.0 / HOUR_STEP;
            self.time += 1.0 / HOUR_STEP;
            if FAECAL_DROP && self.motile == 2 && self.z > 0.0 {
                // println!("Examining poop for shuttle drop!");
                self.z -= (poisson(PROBABILITY_OF_FAECAL_DROP / HOUR_STEP)
                    * (STEP[self.zone][2] as u64)) as f64;
                self.z = limits::max(self.z, 0.0);
            }
            self
        } else {
            if self.z != 0.0 {
                self.z = 0.0;
            }
            self.age += 1.0 / HOUR_STEP;
            self.time += 1.0 / HOUR_STEP;
            self
        }
    }
    fn shuffle_all(vector: Vec<host>) -> Vec<host> {
        vector.into_par_iter().map(|x| x.shuffle()).collect()
    }
    fn dist(host1: &host, host2: &host) -> bool {
        let diff_x: f64 = host1.x - host2.x;
        let diff_y: f64 = host1.y - host2.y;
        let diff_z: f64 = host1.z - host2.z;
        let t: f64 = diff_x.powf(2.0) + diff_y.powf(2.0) + diff_z.powf(2.0);
        /////
        //PRINT STATEMENT
        // if t.powf(0.5)<=TRANSFER_DISTANCE{
        //     println!("{} {} vs {} {}",&host1.x,&host1.y,&host2.x,&host2.y);
        // }
        ////
        let mut transfer_distance: f64 = TRANSFER_DISTANCE;
        if host1.motile == 2 {
            if host2.motile == 2 {
                transfer_distance *= SIZE_FACTOR_FOR_EGGS;
                // if t.powf(0.5)<transfer_distance{println!("Egg to egg infection! @ {:?} vs {:?}, with infection status:{} vs {} respectively",[host1.x,host1.y,host1.z],[host2.x,host2.y,host2.z],host1.infected,host2.infected);}
            } else {
                transfer_distance =
                    TRANSFER_DISTANCE / 2.0 + TRANSFER_DISTANCE * SIZE_FACTOR_FOR_EGGS;
            }
        } else if host2.motile == 2 {
            transfer_distance = TRANSFER_DISTANCE / 2.0 + TRANSFER_DISTANCE * SIZE_FACTOR_FOR_EGGS;
        }
        t.powf(0.5) <= transfer_distance && host1.zone == host2.zone
    }

    fn dist_contaminate(host1: &host, host2: &host) -> bool {
        let diff_x: f64 = host1.x - host2.x;
        let diff_y: f64 = host1.y - host2.y;
        let diff_z: f64 = host1.z - host2.z;
        let t: f64 = diff_x.powf(2.0) + diff_y.powf(2.0) + diff_z.powf(2.0);
        /////
        //PRINT STATEMENT
        // if t.powf(0.5)<=TRANSFER_DISTANCE{
        //     println!("{} {} vs {} {}",&host1.x,&host1.y,&host2.x,&host2.y);
        // }
        ////
        let mut transfer_distance: f64 = CONTAMINATE_TRANSFER_DISTANCE;
        if host1.motile == 2 {
            if host2.motile == 2 {
                transfer_distance *= SIZE_FACTOR_FOR_EGGS;
                // if t.powf(0.5)<transfer_distance{println!("Egg to egg infection! @ {:?} vs {:?}, with infection status:{} vs {} respectively",[host1.x,host1.y,host1.z],[host2.x,host2.y,host2.z],host1.infected,host2.infected);}
            } else {
                transfer_distance = CONTAMINATE_TRANSFER_DISTANCE / 2.0
                    + CONTAMINATE_TRANSFER_DISTANCE * SIZE_FACTOR_FOR_EGGS;
            }
        } else if host2.motile == 2 {
            transfer_distance = CONTAMINATE_TRANSFER_DISTANCE / 2.0
                + CONTAMINATE_TRANSFER_DISTANCE * SIZE_FACTOR_FOR_EGGS;
        }
        t.powf(0.5) <= transfer_distance && host1.zone == host2.zone
    }
    fn transmit(mut inventory: Vec<host>, time: usize) -> Vec<host> {
        // Locate all infected/colonized hosts
        let mut cloneof: Vec<host> = inventory.clone();
        //Infectors
        cloneof = cloneof
            .into_par_iter()
            .filter_map(|mut x| {
                if (x.infected && !COLONIZATION_SPREAD_MODEL)
                    || (COLONIZATION_SPREAD_MODEL && x.colonized)
                {
                    Some(x)
                } else {
                    None
                }
            })
            .collect();
        // println!("Length of infectors is {}",cloneof.len());
        //to be infected
        if COLONIZATION_SPREAD_MODEL {
            inventory = inventory
                .into_par_iter()
                .filter(|x| ((!x.colonized && x.motile == 0) || (!x.infected && x.motile != 0)))
                .collect::<Vec<host>>();
        } else {
            inventory = inventory
                .into_par_iter()
                .filter(|x| !x.infected)
                .collect::<Vec<host>>(); //potentially to save bandwidth, let us remove the concept of colonization in eggs and faeces -> don't need to log colonization in faeces especially!
        }
        //Infection process - both elements concerned here
        inventory = inventory
            .into_par_iter()
            .filter_map(|mut x| {
                for inf in &cloneof {
                    let segment_boundary_condition: bool = !TRANSFERS_ONLY_WITHIN[x.zone]
                        || TRANSFERS_ONLY_WITHIN[x.zone]
                            && x.origin_x == inf.origin_x
                            && x.origin_y == inf.origin_y
                            && x.origin_z == inf.origin_z;
                    // let hosttohost_contact_rules:bool = (HOSTTOHOST_CONTACT_SPREAD || !HOSTTOHOST_CONTACT_SPREAD && !(inf.motile == 0 && x.motile ==0));
                    // let hosttoegg_contact_rules:bool = (HOSTTOEGG_CONTACT_SPREAD || (!HOSTTOEGG_CONTACT_SPREAD && !(inf.motile == 0 && x.motile == 1)));
                    // let hosttofaeces_contact_rules:bool = (HOSTTOFAECES_CONTACT_SPREAD || !HOSTTOFAECES_CONTACT_SPREAD &&!(inf.motile == 0 && x.motile == 2));
                    // let eggtohost_contact_rules:bool = (EGGTOHOST_CONTACT_SPREAD || !EGGTOHOST_CONTACT_SPREAD && !(inf.motile == 1 && x.motile == 0));
                    // let faecestohost_contact_rules:bool = (FAECESTOHOST_CONTACT_SPREAD || !FAECESTOHOST_CONTACT_SPREAD &&!(inf.motile == 2 && x.motile == 0));
                    // let eggtofaeces_contact_rules:bool = (EGGTOFAECES_CONTACT_SPREAD || !EGGTOFAECES_CONTACT_SPREAD && !(inf.motile ==  1 && x.motile == 2));
                    // let faecestoegg_contact_rules:bool = (FAECESTOEGG_CONTACT_SPREAD || !FAECESTOEGG_CONTACT_SPREAD && !(inf.motile ==  2 && x.motile == 1));
                    // let contact_rules:bool = hosttohost_contact_rules && hosttoegg_contact_rules && hosttofaeces_contact_rules && eggtohost_contact_rules && faecestohost_contact_rules && eggtofaeces_contact_rules && faecestoegg_contact_rules;

                    //THIS CONDITION BELOW IS MADE SUCH THAT THE ONLY WAY THE HOST CAN GET INFECTD IS IF IT IS NEAR FAECES AND CONSUMES IT => CONSOLIDATED INTO ONE PROB EVENT
                    if host::dist(inf, &x)
                        && inf.zone == x.zone
                        && segment_boundary_condition
                        && (inf.motile == 2 && x.motile == 0)
                        && !x.infected
                        && inf.zone <= SLAUGHTER_POINT
                    {
                        let before = x.infected.clone();
                        x.infected = x.transfer(1.0);
                        if !before && x.infected {
                            if x.x != 0.0 && x.y != 0.0 {
                                let mut diagnostic: i8 = 1;
                                if x.motile > inf.motile {
                                    diagnostic = -1;
                                }
                                // Access properties of 'inf' here
                                println!(
                                    "{} {} {} {} {} {}",
                                    x.x,
                                    x.y,
                                    x.z,
                                    diagnostic * ((x.motile + 1) as i8) * ((inf.motile + 1) as i8),
                                    time,
                                    x.zone
                                );
                                // if x.zone == 2 && diagnostic*((x.motile+1) as i8) * ((inf.motile+1) as i8)==1{
                                //     println!("INAPPROPRIATE INTERACTION @ zone 2 Delta x : {},Delta y : {},Delta z : {} -> Segments between 2 hosts are the same? : {}",x.x-inf.x,x.y-inf.y,x.z-inf.z,x.origin_x == inf.origin_x&&x.origin_y == inf.origin_y&&x.origin_z == inf.origin_z);
                                //     panic!();
                                // }
                            }
                        }
                    }
                }
                Some(x)
            })
            .collect();
        inventory.extend(cloneof);
        inventory
    }

    fn contaminate(mut inventory: Vec<host>, time: usize) -> Vec<host> {
        // Locate all infected/colonized hosts
        let mut cloneof: Vec<host> = inventory.clone();
        //Infectors
        cloneof = cloneof
            .into_par_iter()
            .filter_map(|mut x| if (x.contaminated) { Some(x) } else { None })
            .collect();
        // println!("Length of infectors is {}",cloneof.len());
        // //to be infected
        // if COLONIZATION_SPREAD_MODEL{
        //     inventory = inventory.into_par_iter().filter(|x| (!x.colonized && x.motile == 0) || (!x.infected && x.motile != 0) ).collect::<Vec<host>>();
        // }else{
        //     inventory = inventory.into_par_iter().filter(|x| !x.infected).collect::<Vec<host>>(); //potentially to save bandwidth, let us remove the concept of colonization in eggs and faeces -> don't need to log colonization in faeces especially!
        // }
        inventory = inventory
            .into_par_iter()
            .filter(|x| !x.contaminated)
            .collect::<Vec<host>>();
        //Infection process - both elements concerned here
        inventory = inventory
            .into_par_iter()
            .filter_map(|mut x| {
                for inf in &cloneof {
                    let segment_boundary_condition: bool = !TRANSFERS_ONLY_WITHIN[x.zone]
                        || TRANSFERS_ONLY_WITHIN[x.zone]
                            && x.origin_x == inf.origin_x
                            && x.origin_y == inf.origin_y
                            && x.origin_z == inf.origin_z;
                    let hosttohost_contact_rules: bool = (HOSTTOHOST_CONTACT_SPREAD
                        || !HOSTTOHOST_CONTACT_SPREAD && !(inf.motile == 0 && x.motile == 0));
                    let hosttoegg_contact_rules: bool = (HOSTTOEGG_CONTACT_SPREAD
                        || (!HOSTTOEGG_CONTACT_SPREAD && !(inf.motile == 0 && x.motile == 1)));
                    let hosttofaeces_contact_rules: bool = (HOSTTOFAECES_CONTACT_SPREAD
                        || !HOSTTOFAECES_CONTACT_SPREAD && !(inf.motile == 0 && x.motile == 2));
                    let eggtohost_contact_rules: bool = (EGGTOHOST_CONTACT_SPREAD
                        || !EGGTOHOST_CONTACT_SPREAD && !(inf.motile == 1 && x.motile == 0));
                    let faecestohost_contact_rules: bool = (FAECESTOHOST_CONTACT_SPREAD
                        || !FAECESTOHOST_CONTACT_SPREAD && !(inf.motile == 2 && x.motile == 0));
                    let eggtofaeces_contact_rules: bool = (EGGTOFAECES_CONTACT_SPREAD
                        || !EGGTOFAECES_CONTACT_SPREAD && !(inf.motile == 1 && x.motile == 2));
                    let faecestoegg_contact_rules: bool = (FAECESTOEGG_CONTACT_SPREAD
                        || !FAECESTOEGG_CONTACT_SPREAD && !(inf.motile == 2 && x.motile == 1));
                    let contact_rules: bool = hosttohost_contact_rules
                        && hosttoegg_contact_rules
                        && hosttofaeces_contact_rules
                        && eggtohost_contact_rules
                        && faecestohost_contact_rules
                        && eggtofaeces_contact_rules
                        && faecestoegg_contact_rules;
                    if host::dist_contaminate(inf, &x)
                        && inf.zone == x.zone
                        && segment_boundary_condition
                        && contact_rules
                        && !x.contaminated
                    {
                        let before = x.contaminated.clone();
                        x.contaminated = roll(x.prob2);
                        if !before && x.contaminated {
                            if x.motile != 0 {
                                x.infected = true;
                            } //"Contaminated deposits are the equivalent to infected deposits"
                            if x.x != 0.0 && x.y != 0.0 {
                                let mut diagnostic: i8 = 1;
                                if x.motile > inf.motile {
                                    diagnostic = -1;
                                }
                                // Access properties of 'inf' here
                                println!(
                                    "{} {} {} {} {} {}",
                                    x.x,
                                    x.y,
                                    x.z,
                                    diagnostic
                                        * (((x.motile + 1) as i8) * ((inf.motile + 1) as i8) + 100),
                                    time,
                                    x.zone
                                );
                                //Diagnostic
                                // panic!(
                                //     "{} {} {} {} {} {} {} {} {}",
                                //     x.x,
                                //     inf.x,
                                //     x.y,
                                //     inf.y,
                                //     x.z,
                                //     inf.z,
                                //     diagnostic*(((x.motile+1) as i8) * ((inf.motile+1) as i8) + 100),
                                //     time,
                                //     x.zone
                                // );
                                // if x.zone == 2 && diagnostic*((x.motile+1) as i8) * ((inf.motile+1) as i8)==1{
                                //     println!("INAPPROPRIATE INTERACTION @ zone 2 Delta x : {},Delta y : {},Delta z : {} -> Segments between 2 hosts are the same? : {}",x.x-inf.x,x.y-inf.y,x.z-inf.z,x.origin_x == inf.origin_x&&x.origin_y == inf.origin_y&&x.origin_z == inf.origin_z);
                                //     panic!();
                                // }
                            }
                        }
                    }
                }
                Some(x)
            })
            .collect();
        inventory.extend(cloneof);
        inventory
    }

    fn cleanup(inventory: Vec<host>) -> (Vec<host>, Vec<host>) {
        inventory
            .into_par_iter()
            .partition(|x| x.motile == 2 || (!COLLECT_DEPOSITS && x.motile == 1))
    }
    fn collect(inventory: Vec<host>) -> [Vec<host>; 2] {
        //hosts and deposits potentially get collected
        let mut collection: Vec<host> = Vec::new();
        let vec1: Vec<host> = inventory
            .into_iter()
            .filter_map(|mut x| {
                // println!("host in zone {}",x.zone);
                if x.motile == 0 && x.age > AGE_OF_HOSTCOLLECTION && x.zone == GRIDSIZE.len() - 1 {
                    // println!("Collecting host(s)...{} days old",x.age/24.0);
                    collection.push(x);
                    None
                } else if x.motile == 1 && x.age > AGE_OF_DEPOSITCOLLECTION {
                    // println!("Collecting deposit(s)...");
                    collection.push(x);
                    None
                } else {
                    Some(x)
                }
            })
            .collect();
        [vec1, collection] //collection vector here to be added and pushed into the original collection vector from the start of the loop! This function merely outputs what should be ADDED to collection!
    }
    fn collect__(inventory: Vec<host>, zone: &mut Zone_3D) -> [Vec<host>; 2] {
        //hosts and deposits potentially get collected
        let mut collection: Vec<host> = Vec::new();
        let vec1: Vec<host> = inventory
            .into_iter()
            .filter_map(|mut x| {
                // println!("host in zone {}",x.zone);
                // println!("GRIDSIZE - 1 is {} ",GRIDSIZE.len()-1);
                // let is_evisceration_checked_at_final_zone:bool =  !EVISCERATE || EVISCERATE_ZONES.contains(&x.zone) == false || (EVISCERATE &&  EVISCERATE_ZONES.contains(&x.zone) && x.eviscerated);
                if x.motile == 0 && x.time > ages[ages.len() - 1] && x.zone == GRIDSIZE.len() - 1 {
                    // println!("Collecting host(s)...{} days old",x.age/24.0);
                    zone.subtract(x.origin_x, x.origin_y, x.origin_z);
                    collection.push(x);
                    // *capacity+=1;
                    None
                } else if x.motile == 1 && x.age > AGE_OF_DEPOSITCOLLECTION && COLLECT_DEPOSITS {
                    // println!("Collecting deposit(s)...");
                    collection.push(x);
                    // *capacity+=1;
                    None
                } else {
                    Some(x)
                }
            })
            .collect();
        [vec1, collection] //collection vector here to be added and pushed into the original collection vector from the start of the loop! This function merely outputs what should be ADDED to collection!
    }
    fn collect_and_replace(inventory: Vec<host>, zone: &mut Zone_3D) -> [Vec<host>; 2] {
        //same as collect but HOSTS GET REPLACED (with a Poisson rate of choosing) - note that this imports hosts, doesn't transfer from earlier zone
        let mut collection: Vec<host> = Vec::new();
        let vec1: Vec<host> = inventory
            .into_iter()
            .filter_map(|mut x| {
                if x.motile == 0 && x.time > ages[ages.len() - 1] && x.zone == GRIDSIZE.len() - 1 {
                    // println!("Zone {} capacity (before): {}",zone.zone,zone.capacity);
                    // println!("Collecting host(s)...{} days old",x.age/24.0);
                    zone.subtract(x.origin_x, x.origin_y, x.origin_z);
                    zone.capacity += 1;
                    // println!("Zone {} capacity (after): {}",zone.zone,zone.capacity);
                    collection.push(x.clone());
                    // None
                    let mut rng = thread_rng();
                    let roll = Uniform::new(0.0, 1.0);
                    let rollnumber: f64 = rng.sample(roll);
                    if rollnumber < PROBABILITY_OF_INFECTION {
                        Some(host {
                            contaminated: false,
                            infected: true,
                            age: normal_(MIN_AGE, MEAN_AGE, STD_AGE, MAX_AGE),
                            time: 0.0,
                            eviscerated: false,
                            ..x
                        })
                    } else {
                        Some(host {
                            contaminated: false,
                            infected: false,
                            age: normal_(MIN_AGE, MEAN_AGE, STD_AGE, MAX_AGE),
                            time: 0.0,
                            eviscerated: false,
                            ..x
                        })
                    }
                } else if x.motile == 1 && x.age > AGE_OF_DEPOSITCOLLECTION {
                    // println!("Collecting deposit(s)...");
                    collection.push(x);
                    None
                } else {
                    Some(x)
                }
            })
            .collect();
        [vec1, collection] //collection vector here to be added and pushed into the original collection vector from the start of the loop! This function merely outputs what should be ADDED to collection!
    }
    fn report(inventory: &Vec<host>) -> [f64; 4] {
        //simple function to quickly return the percentage of infected hosts
        let inf: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.infected && x.motile == 0)
            .collect::<Vec<_>>()
            .len() as f64;
        let noofhosts: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.motile == 0)
            .collect::<Vec<_>>()
            .len() as f64;

        let inf2: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.infected && x.motile == 1)
            .collect::<Vec<_>>()
            .len() as f64;
        let noofhosts2: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.motile == 1)
            .collect::<Vec<_>>()
            .len() as f64;

        [
            inf / (noofhosts + 1.0),
            inf2 / (noofhosts2 + 1.0),
            noofhosts,
            noofhosts2,
        ]
    }
    fn zone_report(inventory: &Vec<host>, zone: usize) -> [f64; 8] {
        //simple function to quickly return the percentage of infected hosts
        //Filter for zone
        let mut inventory: Vec<host> = inventory
            .clone()
            .into_iter()
            .filter(|x| x.zone == zone)
            .collect::<Vec<_>>();
        //Mobile hosts contaminated calculation
        let inf_cont: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.contaminated && x.motile == 0)
            .collect::<Vec<_>>()
            .len() as f64;
        //Mobile hosts infected calculation
        let inf: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.infected && x.motile == 0)
            .collect::<Vec<_>>()
            .len() as f64;
        let noofhosts: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.motile == 0)
            .collect::<Vec<_>>()
            .len() as f64;
        //Consumable immobiles infected calculation
        let inf2: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.infected && x.motile == 1)
            .collect::<Vec<_>>()
            .len() as f64;
        let noofhosts2: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.motile == 1)
            .collect::<Vec<_>>()
            .len() as f64;

        //Colonization
        let inf3: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.colonized && x.motile == 0)
            .collect::<Vec<_>>()
            .len() as f64;

        //Faeces
        let inf4: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.infected && x.motile == 2)
            .collect::<Vec<_>>()
            .len() as f64;
        let noofhosts4: f64 = inventory
            .clone()
            .into_iter()
            .filter(|x| x.motile == 2)
            .collect::<Vec<_>>()
            .len() as f64;

        [
            inf_cont / (noofhosts + 1.0),
            inf / (noofhosts + 1.0),
            inf2 / (noofhosts2 + 1.0),
            noofhosts,
            noofhosts2,
            inf3 / (noofhosts + 1.0),
            inf4 / (noofhosts4 + 1.0),
            noofhosts4,
        ]
    }
    fn generate_in_grid(zone: &mut Zone_3D, hosts: &mut Vec<host>) {
        //Fill up each segment completely to full capacity in a zone with hosts. Also update the capacity to reflect that there is no more space
        let zone_no: usize = zone.clone().zone;
        zone.segments.iter_mut().for_each(|mut x| {
            let mean_x: f64 = ((x.range_x as f64) / 2.0) as f64;
            let std_x: f64 = ((x.range_x as f64) / SPORADICITY) as f64;
            let max_x: f64 = x.range_x as f64;
            let mean_y: f64 = ((x.range_y as f64) / 2.0) as f64;
            let std_y: f64 = ((x.range_y as f64) / SPORADICITY) as f64;
            let max_y: f64 = x.range_y as f64;
            for _ in 0..x.capacity.clone() as usize {
                hosts.push(host::new(
                    zone_no,
                    CONTACT_TRANSMISSION_PROBABILITY[zone_no],
                    x.origin_x as f64 + normal(mean_x, std_x, max_x),
                    (x.origin_y as f64 + normal(mean_y, std_y, max_y)) as f64,
                    x.origin_z as f64,
                    RESTRICTION,
                    x.range_x,
                    x.range_y,
                    x.range_z,
                ));
            }
            x.capacity = 0;
            zone.capacity = 0;
        });
    }
}

fn main() {
    let mut hosts: Vec<host> = Vec::new();
    let mut faecal_inventory: Vec<host> = Vec::new();
    // let mut feast: Vec<host> =  Vec::new();
    let mut contaminants: u64 = 0;
    let mut hosts_in_collection: [u64; 2] = [0, 1];
    let mut colonials_in_collection: [u64; 2] = [0, 1];
    let mut deposits_in_collection: [u64; 2] = [0, 1];
    let mut faecal_collection: [u64; 2] = [0, 1];
    let mut zones: Vec<Zone_3D> = Vec::new();
    //Influx parameter
    let mut influx: bool = INFLUX;
    //Generate eviscerators
    let mut eviscerators: Vec<Eviscerator> = Vec::new();
    if EVISCERATE {
        for index in 0..EVISCERATE_ZONES.len() {
            for set_no in 0..NO_OF_LINES[index] {
                for _ in 0..NO_OF_PROBES[index] {
                    for chronology in 0..NO_OF_EVISCERATORS {
                        //each struct you are pushing represents a probe
                        eviscerators.push(Eviscerator {
                            zone: EVISCERATE_ZONES[index],
                            set: set_no,
                            infected: false,
                            chronology: chronology,
                            number_of_times_infected: 0,
                        })
                    }
                }
            }
        }
    }

    //Initialise with hosts in the first zone only
    for grid in 0..GRIDSIZE.len() {
        zones.push(Zone_3D::generate_empty(
            grid,
            [
                GRIDSIZE[grid][0] as u64,
                GRIDSIZE[grid][1] as u64,
                GRIDSIZE[grid][2] as u64,
            ],
            STEP[grid],
        ));
    }

    // println!("Here are the capacities for each of the zones:{},{},{}",zones[0].capacity, zones[1].capacity,zones[2].capacity);

    host::generate_in_grid(&mut zones[0], &mut hosts);
    // println!("{:?}", hosts.len());
    // for thing in hosts.clone(){
    //     println!("Located at zone {} in {} {}: MOTION PARAMS: {} for {} and {} for {}",thing.zone,thing.x,thing.y,thing.origin_x,thing.range_x,thing.origin_y,thing.range_y);
    // }
    //GENERATE INFECTED HOST
    // hosts.push(host::new_inf(1,CONTACT_TRANSMISSION_PROBABILITY,(GRIDSIZE[0] as u64)/2,(GRIDSIZE[1] as u64)/2),true,STEP as u64,STEP as u64); // the infected
    // hosts = host::infect(hosts,400,400,0);
    // hosts = host::infect(hosts,800,800,0);
    // hosts = host::infect(hosts,130,40,0);
    // hosts = host::infect(hosts,10,10,0);
    // hosts = host::infect(hosts,300,1800,0);

    //MORE EFFICIENT WAY TO INFECT MORE hosts - insize zone 0
    let zone_to_infect: usize = 0;
    // ---- Generating all patient 0s in the center radially
    // hosts = host::infect_multiple(hosts,GRIDSIZE[zone_to_infect][0] as u64/2,GRIDSIZE[zone_to_infect][1] as u64/2,GRIDSIZE[zone_to_infect][2] as u64/2,HOST_0,0, true);
    // ---- Generating all patient 0s at x = 0
    // hosts = host::infect_multiple(hosts,0,GRIDSIZE[zone_to_infect][1] as u64/2,GRIDSIZE[zone_to_infect][2] as u64/2,HOST_0,0, true);
    //----- Generate randomly throughout starting space
    for _ in 0..HOST_0 {
        // Use infect multiple to infect 1 host at a time inside the grid/zone and you can randomly choose the location - here as a uniform distribution
        hosts = host::infect_multiple(
            hosts,
            uniform(0.0, GRIDSIZE[zone_to_infect][0]) as u64,
            uniform(0.0, GRIDSIZE[zone_to_infect][1]) as u64,
            uniform(0.0, GRIDSIZE[zone_to_infect][2]) as u64,
            1,
            0,
            true,
        );
    }

    // println!("Total number of hosts is {}", hosts.len());
    // for segment in &mut zones[0].segments {
    // println!("Segment has coordinates {} {} {}", segment.origin_x, segment.origin_y, segment.origin_z);

    // if segment.origin_x % 8 == 0 {
    //     // println!("")
    //     segment.generate(false, false, 1, &mut hosts);
    //     // segment.generate(true, false, 1, &mut hosts);
    //     // println!("Laying pure at {} {} {}",segment.origin_x,segment.origin_y,segment.origin_z);

    //     if segment.origin_x % 16 == 0 {
    //         segment.generate(true,false,1,&mut hosts);
    //         // println!("Laying impure at {} {} {}",segment.origin_x,segment.origin_y,segment.origin_z);
    //     }
    // }
    // }
    // let mut check:Vec<host> = hosts.clone();
    // check.retain(|x| x.infected);
    // println!("Total number of hosts is {} and total number of hosts infected is {}", hosts.len(),check.len());
    // panic!("Check done! Exiting!");

    //Count number of infected
    // let it: u64 = hosts.clone().into_iter().filter(|x| x.infected).collect()::<Vec<_>>.len();
    // let mut vecc_into: Vec<host> = hosts.clone().into_iter().filter(|x| x.infected).collect::<Vec<_>>(); //With this re are RETAINING the hosts and deposits within the original vector
    // println!("NUMBER OF INFECTED hosts IS {}", vecc_into.len());
    //CSV FILE
    let filestring: String = format!("./output.csv");
    if fs::metadata(&filestring).is_ok() {
        fs::remove_file(&filestring).unwrap();
    }
    // Open the file in append mode for writing
    let mut file = OpenOptions::new()
        .write(true)
        .create(true)
        .append(true) // Open in append mode
        .open(&filestring)
        .unwrap();
    let mut wtr = Writer::from_writer(file);
    for time in 0..LENGTH {
        // println!("Capacity of zone 1 is {}",zones[1].capacity);
        let mut collect: Vec<host> = Vec::new();
        if time % (24 / FAECAL_CLEANUP_FREQUENCY) == 0 {
            (faecal_inventory, hosts) = host::cleanup(hosts);
        }
        // println!("{} CHECK {}",time%(PERIOD_OF_TRANSPORT  as usize),time%(PERIOD_OF_TRANSPORT  as usize) == 0);

        for times in FEED_TIMES {
            if time % 24 == times && time > 0 && (FEED_1 || FEED_2) {
                for spaces in FEED_ZONES {
                    hosts = zones[spaces].clone().feed_setup(hosts, time.clone());
                }
            }
        }
        //Periodically cleaning eviscerators
        if (time == 4 || time == 7) && CLEAN_EVISCERATORS {
            // println!("Cleaning eviscerators!");
            eviscerators.iter_mut().for_each(|mut ev| {
                ev.infected = false;
            })
        }

        if EVISCERATE {
            let mut counter: usize = 0;
            for zone in EVISCERATE_ZONES {
                // println!("Evisceration occurring at zone {}",zone);
                let no: usize = hosts
                    .clone()
                    .into_iter()
                    .filter(|x| x.motile == 0 && x.zone == zone && !x.eviscerated)
                    .collect::<Vec<_>>()
                    .len()
                    / NO_OF_LINES[counter];
                // for set_no in 0..NO_OF_LINES[counter]{
                //     zones[zone].multi_eviscerate(&mut eviscerators,&mut hosts,time.clone(),no,set_no);
                // }
                zones[zone].multi_eviscerate(
                    &mut eviscerators,
                    &mut hosts,
                    time.clone(),
                    counter,
                    no,
                    NO_OF_LINES[counter],
                );
                counter += 1;
                // println!("{} hosts have been eviscerated and infected so far",hosts.clone().into_iter().filter(|x| x.eviscerated && x.infected).collect::<Vec<_>>().len() as u64);
            }
        }

        if time % (PERIOD_OF_TRANSPORT as usize) == 0 {
            // println!("Fulfilling period of transport right now");
            host::transport(&mut hosts, &mut zones, influx);
            // println!("Total number of hosts is {}: Total number of faeces is {}",  hosts.clone().into_iter().filter(|x| x.motile == 0).collect::<Vec<_>>().len() as u64,hosts.clone().into_iter().filter(|x| x.motile == 2).collect::<Vec<_>>().len() as u64)
        }

        let mut FinalZone: &mut Zone_3D = &mut zones[GRIDSIZE.len() - 1];
        [hosts, collect] = host::collect__(hosts, &mut FinalZone);

        for unit in 0..HOUR_STEP as usize {
            // println!("Number of poop is {}",hosts.clone().into_iter().filter(|x| x.motile == 2).collect::<Vec<_>>().len() as u64);
            hosts = host::shuffle_all(hosts);
            hosts = host::transmit(hosts, time.clone());
            hosts = host::contaminate(hosts, time.clone());
            if FLY && unit != 0 && (unit % FLY_FREQ as usize) == 0 {
                hosts = host::land(hosts);
            }
        } //Say hosts move/don't move every 15min - 4 times per hour
        host::recover(&mut hosts);
        hosts = host::deposit_all(hosts);
        //Collect the hosts and deposits as according
        // println!("Number of infected eggs in soon to be collection is {}",collect.clone().into_iter().filter(|x| x.motile == 1 && x.infected).collect::<Vec<_>>().len() as f64);
        // feast.append(&mut collect);
        //Update Collection numbers
        let no_of_contaminated_hosts: u64 = collect
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 0 && x.contaminated)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_infected_hosts: u64 = collect
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 0 && x.infected)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_colonized_hosts: u64 = collect
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 0 && x.colonized)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_hosts: u64 = collect
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 0)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_deposits: u64 = collect
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 1)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_infected_deposits: u64 = collect
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 1 && x.infected)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_faeces: u64 = faecal_inventory
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 2)
            .collect::<Vec<_>>()
            .len() as u64;
        let no_of_infected_faeces: u64 = faecal_inventory
            .clone()
            .into_par_iter()
            .filter(|x| x.motile == 2 && x.infected)
            .collect::<Vec<_>>()
            .len() as u64;

        contaminants += no_of_contaminated_hosts;
        hosts_in_collection[0] += no_of_infected_hosts;
        colonials_in_collection[0] += no_of_colonized_hosts;
        hosts_in_collection[1] += no_of_hosts;
        colonials_in_collection[1] += no_of_hosts;
        deposits_in_collection[0] += no_of_infected_deposits;
        deposits_in_collection[1] += no_of_deposits;
        faecal_collection[0] += no_of_infected_faeces;
        faecal_collection[1] += no_of_faeces;

        if INFLUX && time % PERIOD_OF_INFLUX as usize == 0 && time > 0 {
            influx = true;
            // println!("Influx just got changed to true");
        } else {
            influx = false;
        }
        // let mut count:u8 = 0;
        // for i in zones.clone(){
        //     println!("{} : {}", count,i.capacity);
        //     count+=1;
        // }
        //Farm
        let no_of_zones: usize = GRIDSIZE.len();
        let collection_zone_no: u8 = no_of_zones as u8 + 1;
        //Call once
        for iter in 0..no_of_zones {
            let [mut perc_cont, mut perc, mut perc2, mut total_hosts, mut total_hosts2, mut perc3, mut perc4, mut total_hosts4] =
                host::zone_report(&hosts, iter);
            let no_cont = perc_cont.clone() * total_hosts;
            perc_cont *= 100.0;
            let no = perc.clone() * total_hosts;
            perc = perc * 100.0;
            let no2 = perc2.clone() * total_hosts2;
            perc2 = perc2 * 100.0;
            let no3 = perc3.clone() * total_hosts;
            perc3 *= 100.0;
            let no4 = perc4.clone() * total_hosts4;
            perc4 = perc4 * 100.0;
            wtr.write_record(&[
                perc_cont.to_string(),
                total_hosts.to_string(),
                no_cont.to_string(),
                perc.to_string(),
                total_hosts.to_string(),
                no.to_string(),
                perc2.to_string(),
                total_hosts2.to_string(),
                no2.to_string(),
                perc3.to_string(),
                no3.to_string(),
                perc4.to_string(),
                total_hosts4.to_string(),
                no4.to_string(),
                format!("Zone {}", iter),
            ]);
        }

        //Collection
        // let [mut _perc,mut _perc2,mut _total_hosts,mut _total_hosts2] = host::report(&feast);
        let _no = hosts_in_collection[0];
        let _perc = (hosts_in_collection[0] as f64) / (hosts_in_collection[1] as f64) * 100.0;
        let _perc_cont = (contaminants as f64) / (hosts_in_collection[1] as f64) * 100.0;
        let _no2 = deposits_in_collection[0];
        let _perc2 =
            (deposits_in_collection[0] as f64) / (deposits_in_collection[1] as f64) * 100.0;
        let _total_hosts = hosts_in_collection[1];
        let _total_hosts2 = deposits_in_collection[1];
        let _no3 = colonials_in_collection[0];
        let _perc3 =
            (colonials_in_collection[0] as f64) / (colonials_in_collection[1] as f64) * 100.0;
        let _no4 = faecal_collection[0];
        let _perc4 = (faecal_collection[0] as f64) / (faecal_collection[1] as f64) * 100.0;
        let _total_faeces = faecal_collection[1];
        // println!("{} {} {} {} {} {}",perc,total_hosts,no,perc2,total_hosts2,no2);
        // println!("{} {} {} {} {} {} {} {} {} {} {} {}",perc,total_hosts,no,perc2,total_hosts2,no2,_perc,_total_hosts,_no,_perc2,_total_hosts2,_no2);
        wtr.write_record(&[
            _perc_cont.to_string(),
            _total_hosts.to_string(),
            contaminants.to_string(),
            _perc.to_string(),
            _total_hosts.to_string(),
            _no.to_string(),
            _perc2.to_string(), //Eggs
            _total_hosts2.to_string(),
            _no2.to_string(),
            _perc3.to_string(), //Colonized Hosts
            _no3.to_string(),
            _perc4.to_string(), //faeces
            _total_faeces.to_string(),
            _no4.to_string(),
            "Collection Zone".to_string(),
        ])
        .unwrap();

        // if host::report(&hosts)[2]<5.0{break;}
    }
    wtr.flush().unwrap();
    // println!("{} {} {} {} {} {}",STEP[0][0],STEP[0][1],STEP[0][2],LENGTH,GRIDSIZE.len(), TRANSFER_DISTANCE); //Last 5 lines are going to be zone config lines that need to be picked out in plotter.py
    for zone in 0..GRIDSIZE.len() {
        println!(
            "{} {} {} {} {} {}",
            GRIDSIZE[zone][0], GRIDSIZE[zone][1], GRIDSIZE[zone][2], 1000, 0, zone
        );
    }
    for zone in 0..GRIDSIZE.len() {
        // println!("{} {} {} {} {} {}",GRIDSIZE[zone][0],GRIDSIZE[zone][1],GRIDSIZE[zone][2],1000,0,zone);
        println!(
            "{} {} {} {} {} {}",
            STEP[zone][0] + 100000,
            STEP[zone][1] + 100000,
            STEP[zone][2] + 100000,
            GRIDSIZE[zone][0] + 100000.0,
            GRIDSIZE[zone][1] + 100000.0,
            GRIDSIZE[zone][2] + 100000.0 + zone as f64
        ); //Paramters for R file to extract and plot
    }
    // println!("{} {} {} {} {} {}",GRIDSIZE[0][0],GRIDSIZE[0][1],GRIDSIZE[0][2],0,0,0); //Last 5 lines are going to be zone config lines that need to be picked out in plotter.py

    // Open a file for writing
    let mut file = File::create("parameters.txt").expect("Unable to create file");

    // Write constants to the file
    // Space
    writeln!(file, "## Space").expect("Failed to write to file");
    writeln!(
        file,
        "- RESTRICTION: {} (Are the hosts restricted to segments within each zone)",
        RESTRICTION
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- INFECTION PROBABILITIES PER ZONE: {:?} (Probability of transfer of salmonella per zone)",
        LISTOFPROBABILITIES
    )
    .expect("Failed to write to file");
    writeln!(file, "- CONTACT TRANSMISSION PROBABILITIES PER ZONE: {:?} (Probability of transfer of salmonella per zone)", CONTACT_TRANSMISSION_PROBABILITY).expect("Failed to write to file");
    writeln!(file, "- GRIDSIZE: {:?} (Size of the grid)", GRIDSIZE)
        .expect("Failed to write to file");
    writeln!(file, "- MAX_MOVE: {} (Maximum move value)", MAX_MOVE)
        .expect("Failed to write to file");
    writeln!(file, "- MEAN_MOVE: {} (Mean move value)", MEAN_MOVE)
        .expect("Failed to write to file");
    writeln!(
        file,
        "- STD_MOVE: {} (Standard deviation of move value)",
        STD_MOVE
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- MAX_MOVE_Z: {} (Maximum move for vertical motion)",
        MAX_MOVE_Z
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- MEAN_MOVE_Z: {} (Mean move value for vertical motion)",
        MEAN_MOVE_Z
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- STD_MOVE_Z: {} (Standard deviation of move value for vertical motion)",
        STD_MOVE_Z
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- FAECAL DROP: {} (Does faeces potentially fall between segments downwards?)",
        FAECAL_DROP
    )
    .expect("Failed to write to file");
    writeln!(file, "- PROBABILITY OF FAECAL DROP: {} (If yes, what is the probability? -> Poisson hourly rate)", PROBABILITY_OF_FAECAL_DROP).expect("Failed to write to file");
    writeln!(file, "- TRANSMISSION BETWEEN ZONES disabled: {:?} (Can diseases transfer between segments/cages within each zone?)", TRANSFERS_ONLY_WITHIN).expect("Failed to write to file");
    // Fly configuration
    writeln!(file, "\n## Flight module").expect("Failed to write to file");
    writeln!(file, "- FLY: {} (Flight module enabled/disabled)", FLY)
        .expect("Failed to write to file");
    if FLY {
        writeln!(
            file,
            "- FLY_FREQ: {} (Frequency of flight - which HOUR STEP do the hosts land, if at all)",
            FLY_FREQ
        )
        .expect("Failed to write to file");
    }

    //Perching configuration
    writeln!(file, "\n## Perching module").expect("Failed to write to file");
    writeln!(file, "- PERCH: {} (Do the hosts perch?)", PERCH).expect("Failed to write to file");
    if PERCH {
        writeln!(
            file,
            "- PERCH_HEIGHT: {} (Periodic height at which hosts are able to perch to)",
            PERCH_HEIGHT
        )
        .expect("Failed to write to file");
        writeln!(
            file,
            "- PERCH_FREQ: {} (Frequency/Probability of perching)",
            PERCH_FREQ
        )
        .expect("Failed to write to file");
        writeln!(
            file,
            "- DEPERCH_FREQ: {} (Frequency/Probability of ceasing perching)",
            DEPERCH_FREQ
        )
        .expect("Failed to write to file");
        writeln!(
            file,
            "- PERCH_ZONES: {:?} (Zones in which perching occurs)",
            PERCH_ZONES
        )
        .expect("Failed to write to file");
    }

    // Eviscerator configuration
    writeln!(
        file,
        "\n## Eviscerator Configuration enabled:{}",
        EVISCERATE
    )
    .expect("Failed to write to file");
    writeln!(file, "- Evisceration Zones: {:?}", EVISCERATE_ZONES)
        .expect("Failed to write to file");
    writeln!(file, "- NUMBER OF EVISCERATORS PROBES: {:?}", NO_OF_PROBES)
        .expect("Failed to write to file");
    writeln!(
        file,
        "- NUMBER OF EVISCERATOR MACHINES (each containing said amount of probes above): {:?}",
        NO_OF_EVISCERATORS
    )
    .expect("Failed to write to file");
    writeln!(file, "- EVISCERATOR DECAY: {} (Number of hosts an eviscerator has to go through before the infection is gone)", EVISCERATE_DECAY).expect("Failed to write to file");
    writeln!(
        file,
        "- MISHAP: {} (Can hosts explode by accident during evisceration??)",
        MISHAP
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- MISHAP_PROBABILITY: {} (At what probability does this accident happen??)",
        MISHAP_PROBABILITY
    )
    .expect("Failed to write to file");
    writeln!(file, "- MISHAP_RADIUS: {} (At what radius does this explosion occur - (currently fixed radius)??)", MISHAP_RADIUS).expect("Failed to write to file");
    if CLEAN_EVISCERATORS {
        writeln!(file, "EVISCERATORS ARE CLEANED IN THIS SIMULATION")
            .expect("Failed to write to file");
    }

    // Transfer config
    writeln!(file, "\n## Transfer Configuration").expect("Failed to write to file");
    writeln!(
        file,
        "- Times Manual Map: {:?} (Times that hosts have to spend in each zone)",
        ages
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- Influx?: {} (Did the simulation bring in hosts to process?)",
        INFLUX
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- If yes, they were brought in every {} hours",
        PERIOD_OF_INFLUX
    )
    .expect("Failed to write to file");
    writeln!(file, "- Period of transport rules : {} hours (How many hours until we check to see if hosts need to be moved from zone to zone)", PERIOD_OF_TRANSPORT).expect("Failed to write to file");

    // Disease
    writeln!(file, "\n## Disease").expect("Failed to write to file");
    writeln!(
        file,
        "- TRANSFER_DISTANCE: {} (Maximum distance for disease transmission)",
        TRANSFER_DISTANCE
    )
    .expect("Failed to write to file");

    // Collection
    writeln!(file, "\n## Collection").expect("Failed to write to file");
    writeln!(
        file,
        "- AGE_OF_HOSTCOLLECTION: {} days",
        AGE_OF_HOSTCOLLECTION / 24.0
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- AGE_OF_DEPOSITCOLLECTION: {} days",
        AGE_OF_DEPOSITCOLLECTION / 24.0
    )
    .expect("Failed to write to file");
    writeln!(
        file,
        "- FAECAL_CLEANUP_FREQUENCY: {} times per day",
        24 / FAECAL_CLEANUP_FREQUENCY
    )
    .expect("Failed to write to file");

    // Resolution
    writeln!(file, "\n## Resolution").expect("Failed to write to file");
    writeln!(file, "- STEP: {:?} (hosts per unit distance)", STEP)
        .expect("Failed to write to file");
    writeln!(file, "- HOUR_STEP: {} (hosts move per hour)", HOUR_STEP)
        .expect("Failed to write to file");
    writeln!(file, "- LENGTH: {} (Simulation duration in hours)", LENGTH)
        .expect("Failed to write to file");

    //Generation
    writeln!(file, "\n## Generation").expect("Failed to write to file");
    writeln!(file, "- SPORADICITY: {} ( Bigger number makes the spread of hosts starting point more even per seg)", SPORADICITY).expect("Failed to write to file");

    //Contamination Pathway Toolbox options
    writeln!(file, "\n##Contamination Pathway Toolbox options").expect("Failed to write to file");
    writeln!(file, "HOSTTOHOST_CONTACT_SPREAD: {} ( Whether contamination can spread from mobile host to mobile host)", HOSTTOHOST_CONTACT_SPREAD).expect("Failed to write to file");
    writeln!(file, "HOSTTOEGG_CONTACT_SPREAD: {} ( Whether contamination can spread from mobile host to edible/consumable deposit from host)", HOSTTOEGG_CONTACT_SPREAD).expect("Failed to write to file");
    writeln!(file, "HOSTTOFAECES_CONTACT_SPREAD: {} ( Whether contamination can spread from mobile host to inedible/non-consumable deposit from host - i.e. faecal matter)", HOSTTOFAECES_CONTACT_SPREAD).expect("Failed to write to file");
    writeln!(file, "EGGTOFAECES_CONTACT_SPREAD: {} ( Whether contamination can spread from edible/consumable deposit from host to inedible/non-consumable deposit from host)", EGGTOFAECES_CONTACT_SPREAD).expect("Failed to write to file");
    writeln!(file, "FAECESTOEGG_CONTACT_SPREAD: {} ( Whether contamination can spread from inedible/non-consumable deposit from host to edible/consumable deposit from host)", FAECESTOEGG_CONTACT_SPREAD).expect("Failed to write to file");
    writeln!(file, "FAECESTOHOST_CONTACT_SPREAD: {} ( Whether contamination can spread from inedible/non-consumable deposit from host to mobile host)", FAECESTOHOST_CONTACT_SPREAD).expect("Failed to write to file");
}
