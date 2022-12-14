use rand::Rng;
use std::{io, str::FromStr};

macro_rules! parse_input {
    ($x:expr, $t:ident) => {
        $x.trim().parse::<$t>().unwrap()
    };
}

fn distance(a: &Position, b: &Position) -> usize {
    ((b.0 as i32 - a.0 as i32).abs() + (b.1 as i32 - a.1 as i32).abs()) as usize
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Owner {
    ME = 1,
    OPP = 0,
    NEUTRAL = -1,
}

impl FromStr for Owner {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1" => Ok(Owner::ME),
            "0" => Ok(Owner::OPP),
            "-1" => Ok(Owner::NEUTRAL),
            _ => Err(()),
        }
    }
}

type Position = (usize, usize);

#[derive(Debug, Clone)]
struct Tile {
    x: usize,
    y: usize,
    scrap: usize,
    owner: Owner,
    units: usize,
    is_recycler: bool,
    is_recycler_near: bool,
    can_build: bool,
    can_spawn: bool,
    neighbors: Vec<(usize, usize)>,
}

#[derive(Debug)]
struct Game {
    w: usize,
    h: usize,
    tiles: Vec<Vec<Tile>>,
}

impl Game {
    fn new() -> Game {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let inputs = input_line.split(" ").collect::<Vec<_>>();

        // INIT SIZE & TILE LIST

        let mut game = Game {
            w: parse_input!(inputs[0], usize),
            h: parse_input!(inputs[1], usize),
            tiles: vec![],
        };

        // INIT TILES
        for x in 0..game.w {
            game.tiles.push(vec![]);
            for y in 0..game.h {
                let mut new_tile = Tile {
                    x,
                    y,
                    scrap: 0,
                    owner: Owner::NEUTRAL,
                    units: 0,
                    is_recycler: false,
                    is_recycler_near: false,
                    can_build: true,
                    can_spawn: true,
                    neighbors: vec![],
                };

                if x > 0 {
                    new_tile.neighbors.push((x - 1, y));
                }
                if x + 1 < game.w {
                    new_tile.neighbors.push((x + 1, y));
                }
                if y > 0 {
                    new_tile.neighbors.push((x, y - 1));
                }
                if y + 1 < game.h {
                    new_tile.neighbors.push((x, y + 1));
                }

                game.tiles[x].push(new_tile);
            }
        }

        game
    }

    fn parse_matter(&self) -> [usize; 2] {
        let mut input_line = String::new();
        io::stdin().read_line(&mut input_line).unwrap();
        let inputs = input_line.split(" ").collect::<Vec<_>>();

        [
            parse_input!(inputs[0], usize),
            parse_input!(inputs[1], usize),
        ]
    }

    fn parse_tiles(&mut self) -> [Vec<(usize, usize)>; 3] {
        let mut my_units: Vec<(usize, usize)> = vec![];
        let mut my_spawnables: Vec<(usize, usize)> = vec![];
        let mut my_buildables: Vec<(usize, usize)> = vec![];

        for y in 0..self.h {
            for x in 0..self.w {
                let mut input_line = String::new();
                io::stdin().read_line(&mut input_line).unwrap();
                let inputs = input_line.split(" ").collect::<Vec<_>>();

                let mut tile = self.tiles[x].get_mut(y).unwrap();

                tile.scrap = parse_input!(inputs[0], usize);
                tile.owner = parse_input!(inputs[1], Owner);
                tile.units = parse_input!(inputs[2], usize);
                tile.is_recycler = parse_input!(inputs[3], u8) != 0;
                tile.can_build = parse_input!(inputs[4], u8) != 0;
                tile.can_spawn = parse_input!(inputs[5], u8) != 0;
                tile.is_recycler_near = parse_input!(inputs[6], u8) != 0;

                if tile.units > 0 && tile.owner == Owner::ME {
                    my_units.push((x, y));
                }
                if tile.can_spawn && tile.owner == Owner::ME {
                    my_spawnables.push((x, y));
                }
                if tile.can_build && tile.owner == Owner::ME {
                    my_buildables.push((x, y));
                }
            }
        }

        [my_units, my_spawnables, my_buildables]
    }
}

fn main() {
    let mut rng = rand::thread_rng();
    // INIT GAME STRUCT
    let mut game = Game::new();

    // MAIN GAME LOOP
    loop {
        // PARSE MATTER VALUES
        let [mut matter, matter_opp] = game.parse_matter();

        // PARSE TILES
        let [mut my_units, mut my_spawnables, mut my_buildables] = game.parse_tiles();

        // INIT THE ACTIONS ARRAY
        let mut actions: Vec<String> = vec![];

        // MOVEMENT MANAGEMENT
        for unit in my_units.into_iter().map(|(x, y)| &game.tiles[x][y]) {
            let mut filtered = unit
                .neighbors
                .clone()
                .into_iter()
                .filter_map(|(x1, y1)| {
                    if game.tiles[x1][y1].scrap != 0 && game.tiles[x1][y1].owner != Owner::ME {
                        Some(&game.tiles[x1][y1])
                    } else {
                        None
                    }
                })
                .collect::<Vec<&Tile>>();

            // eprintln!("{} {} has {} valid sides", unit.x, unit.y, filtered.len());
            if filtered.len() > 0 {
                if filtered.len() > unit.units {
                    for _ in 0..unit.units {
                        let i = rng.gen_range(0..filtered.len());
                        let mut drained = filtered.drain(i..(i + 1));
                        let target = drained.next().unwrap();

                        actions.push(format!(
                            "MOVE 1 {} {} {} {}",
                            unit.x, unit.y, target.x, target.y
                        ));
                    }
                } else {
                    for (i, tile) in filtered.iter().enumerate() {
                        let mut qty = unit.units / filtered.len();
                        if i < unit.units % filtered.len() {
                            qty += 1;
                        }

                        if qty > 0 {
                            actions.push(format!(
                                "MOVE {} {} {} {} {}",
                                qty, unit.x, unit.y, tile.x, tile.y
                            ));
                        }
                    }
                }
            } else if unit.neighbors.len() > 0 {
                if unit.neighbors.len() > unit.units {
                    let mut cloned = unit.neighbors.clone();
                    for _ in 0..unit.units {
                        let i = rng.gen_range(0..cloned.len());
                        let mut drained = cloned.drain(i..(i + 1));
                        let target = drained.next().unwrap();

                        actions.push(format!(
                            "MOVE 1 {} {} {} {}",
                            unit.x, unit.y, target.0, target.1
                        ));
                    }
                } else {
                    for (i, (x1, y1)) in unit.neighbors.iter().enumerate() {
                        let mut qty = unit.units / unit.neighbors.len();
                        if i < unit.units % unit.neighbors.len() {
                            qty += 1;
                        }

                        if qty > 0 {
                            actions
                                .push(format!("MOVE {} {} {} {} {}", qty, unit.x, unit.y, x1, y1));
                        }
                    }
                }
            }
        }

        // BUILD MANAGEMENT
        for tile in my_buildables.into_iter().map(|(x, y)| &game.tiles[x][y]) {
            if matter < 10 {
                break;
            }
            let mut filtered = tile
                .neighbors
                .clone()
                .into_iter()
                .filter_map(|(x1, y1)| {
                    if game.tiles[x1][y1].scrap != 0 && game.tiles[x1][y1].owner == Owner::OPP {
                        Some(&game.tiles[x1][y1])
                    } else {
                        None
                    }
                })
                .collect::<Vec<&Tile>>();

            if filtered.len() != 0 {
                matter -= 10;

                actions.push(format!("BUILD {} {}", tile.x, tile.y));
            }
        }

        // SPAWN MANAGEMENT
        for tile in my_spawnables.into_iter().map(|(x, y)| &game.tiles[x][y]) {
            if matter < 10 {
                break;
            }
            let mut filtered = tile
                .neighbors
                .clone()
                .into_iter()
                .filter_map(|(x1, y1)| {
                    if game.tiles[x1][y1].scrap != 0 && game.tiles[x1][y1].owner != Owner::ME {
                        Some(&game.tiles[x1][y1])
                    } else {
                        None
                    }
                })
                .collect::<Vec<&Tile>>();

            if filtered.len() > 0 {
                let qty = filtered.len().min(matter / 10);
                matter -= qty * 10;

                actions.push(format!("SPAWN {} {} {}", qty, tile.x, tile.y));
            }
        }

        // PRINT ACTION(S)
        if actions.len() > 0 {
            println!("{}", actions.join(";"));
        } else {
            println!("WAIT");
        }
    }
}
