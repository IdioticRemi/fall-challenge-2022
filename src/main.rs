use rand::Rng;
use std::{io, str::FromStr};
use std::collections::{HashMap, VecDeque};

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

    fn get_weight_map(&self) -> HashMap<Position, i32> {
        let mut map: HashMap<Position, i32> = HashMap::new();
        let mut queue: VecDeque<Position> = VecDeque::new();

        for y in 0..self.h {
            for x in 0..self.w {
                queue.push_back((x, y));
            }
        }

        while queue.len() > 0 {
            let (x, y) = queue.pop_front().unwrap();
            let tile = &self.tiles[x][y];

            if tile.scrap > 0 && tile.owner != Owner::ME {
                map.insert((x, y), 0);
                continue;
            }

            let tmp = tile.neighbors.iter().filter_map(|(x1, y1)| map.get(&(*x1, *y1)).map(|t| Some(t))).collect::<Vec<Option<&i32>>>();

            if tmp.len() == 0 {
                queue.push_back((x, y));
                continue;
            }

            let min = tmp.into_iter().min().unwrap().unwrap();

            map.insert((x, y), min + 1);
        }

        map
    }
}

fn main() {
    // let mut rng = rand::thread_rng();
    // INIT GAME STRUCT
    let mut game = Game::new();
    let mut my_side = 0;

    // MAIN GAME LOOP
    loop {
        // PARSE MATTER VALUES
        let [mut matter, _] = game.parse_matter();

        // PARSE TILES
        let [mut my_units, mut my_spawnables, mut my_buildables] = game.parse_tiles();

        // SET MY_SIDE VALUE
        if my_side == 0 && my_units.len() > 0 {
            my_side = if my_units[0].0 > game.w / 2 { 1 } else { -1 };
        }

        // GET WEIGHT MAP
        let weight_map = game.get_weight_map();

        // INIT THE ACTIONS ARRAY
        let mut actions: Vec<String> = vec![];

        // MOVEMENT MANAGEMENT
        for unit in my_units.into_iter().map(|(x, y)| &game.tiles[x][y]) {
            let mut filtered = unit
                .neighbors
                .iter()
                .filter_map(|(x1, y1)| {
                    // Filter going to the enemy tiles
                    let tile = &game.tiles[*x1][*y1];

                    if tile.scrap != 0 && !tile.is_recycler {
                        Some(tile)
                    } else {
                        None
                    }
                })
                .collect::<Vec<&Tile>>();

            filtered.sort_by_key(|tile| weight_map[&(tile.x, tile.y)]);

            let mut path_count = filtered.len();

            if path_count > 0 {
                for (i, tile) in filtered.iter().enumerate() {
                    let qty = unit.units / path_count + (i < unit.units % path_count) as usize;

                    if qty > 0 {
                        actions.push(format!(
                            "MOVE {} {} {} {} {}",
                            qty, unit.x, unit.y, tile.x, tile.y
                        ));
                    }
                }
            }
        }

        // BUILD MANAGEMENT
        let mut sorted_builds = my_buildables.into_iter().map(|(x, y)| (&game.tiles[x][y], weight_map[&(x, y)])).collect::<Vec<(&Tile, i32)>>();
        sorted_builds.sort_by(|(tile_a, va), (tile_b, vb)| {
            distance(&(tile_a.x, tile_a.y), &(game.w / 2, game.h / 2)).cmp(&distance(&(tile_b.x, tile_b.y), &(game.w / 2, game.h / 2)))
        });
        for (tile, _) in sorted_builds {
            if matter < 10 {
                break;
            }
            let opp_neighbors_cnt = tile
                .neighbors
                .iter()
                .filter_map(|(x1, y1)| {
                    if game.tiles[*x1][*y1].scrap > 0 && game.tiles[*x1][*y1].owner == Owner::OPP {
                        Some(true)
                    } else {
                        None
                    }
                }).count();

            if opp_neighbors_cnt > 0 {
                matter -= 10;

                actions.push(format!("BUILD {} {}", tile.x, tile.y));
            }
        }

        // SPAWN MANAGEMENT
        let mut sorted_spawns = my_spawnables.into_iter().map(|(x, y)| (&game.tiles[x][y], weight_map[&(x, y)])).collect::<Vec<(&Tile, i32)>>();
        sorted_spawns.sort_by(|(tile_a, va), (tile_b, vb)| {
            distance(&(tile_a.x, tile_a.y), &(game.w / 2, game.h / 2)).cmp(&distance(&(tile_b.x, tile_b.y), &(game.w / 2, game.h / 2)))
        });

        for (tile, _) in sorted_spawns {
            if matter < 10 {
                break;
            }

            let neutral_neighbors_cnt = tile
                .neighbors
                .iter()
                .filter_map(|(x1, y1)| {
                    if game.tiles[*x1][*y1].scrap > 0 && game.tiles[*x1][*y1].owner == Owner::NEUTRAL {
                        Some(true)
                    } else {
                        None
                    }
                }).count();

            if neutral_neighbors_cnt > 0 {
                matter -= 10;

                actions.push(format!("SPAWN 1 {} {}", tile.x, tile.y));
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
