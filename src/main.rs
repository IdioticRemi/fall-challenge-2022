use std::{io, str::FromStr, cmp::Ordering};
use std::collections::{HashMap, VecDeque, HashSet};
use std::time::Instant;
// use rand::Rng;

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

impl ToString for Owner {
    fn to_string(&self) -> String {
        match self {
            Owner::ME => ".".to_string(),
            Owner::OPP => "x".to_string(),
            Owner::NEUTRAL => "-".to_string(),
        }
    }
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

impl Ord for Owner {
    fn cmp(&self, other: &Self) -> Ordering {
        match (*self, *other) {
            (Owner::ME, Owner::ME) => Ordering::Equal,
            (Owner::ME, _) => Ordering::Greater,
            (_, Owner::ME) => Ordering::Less,
            (Owner::NEUTRAL, Owner::NEUTRAL) => Ordering::Equal,
            (Owner::NEUTRAL, _) => Ordering::Greater,
            (_, Owner::NEUTRAL) => Ordering::Less,
            (Owner::OPP, Owner::OPP) => Ordering::Equal,
        }
    }
}

impl PartialOrd for Owner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
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
        // let mut rng = rand::thread_rng();

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

                new_tile.neighbors.sort_by_key(|(nx, ny)| distance(&(*nx, *ny), &(game.w / 2, game.h / 2)));

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

    fn get_recycler_worth(&self, pos: Position) -> usize {
        let center = &self.tiles[pos.0][pos.1];

        if center.is_recycler_near || center.neighbors.iter().filter(|(x, y)| self.tiles[*x][*y].is_recycler_near).count() > 0 {
            // If center or one of the neighbors has a recycler near, return zero worth
            return 0;
        }

        let mut side_worth = center.neighbors.iter().map(|(x, y)| self.tiles[*x][*y].scrap).collect::<Vec<usize>>();
        let mut center_worth = center.scrap;

        if side_worth.iter().filter(|v| v <= &&center_worth).count() > 2 {
            // If it destroys more than two tiles, return zero worth
            return 0;
        }

        let mut worth: usize = 0;

        while center_worth > 0 {
            worth += 1;

            for i in 0..side_worth.len() {
                if side_worth[i] > 0 {
                    side_worth[i] -= 1;
                    worth += 1;
                }
            }

            center_worth -= 1;
        }

        worth
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

            if tile.scrap > 0 && tile.owner == Owner::OPP {
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

    fn get_map_zones(&self) -> HashMap<Position, Option<(Owner, i32)>> {
        let mut map: HashMap<Position, Option<(Owner, i32)>> = HashMap::new();
        let mut area_id = 0;

        for y in 0..self.h {
            for x in 0..self.w {
                if map.contains_key(&(x, y)) {
                    continue;
                }
                if self.tiles[x][y].scrap == 0 || self.tiles[x][y].is_recycler {
                    map.insert((x, y), None);
                    continue;
                }

                let (mapped, owner) = self.get_area((x, y));

                for (x1, y1) in mapped.iter() {
                    map.insert((*x1, *y1), Some((owner, area_id)));
                }

                area_id += 1;
            }
        }

        map
    }

    fn get_area(&self, start: Position) -> (HashSet<Position>, Owner) {
        let mut queue: VecDeque<Position> = VecDeque::new();
        let mut visited: HashSet<Position> = HashSet::new();
        let mut zone_owner = self.tiles[start.0][start.1].owner;

        queue.push_back(start);

        while queue.len() > 0 {
            let (x, y) = queue.pop_front().unwrap();
            let tile = &self.tiles[x][y];

            if visited.contains(&(x, y)) {
                continue;
            }

            visited.insert((x, y));

            if tile.owner == Owner::OPP {
                zone_owner = Owner::OPP
            } else if zone_owner == Owner::ME && tile.owner == Owner::NEUTRAL {
                zone_owner = Owner::NEUTRAL
            }

            for next in tile.neighbors.iter().filter(|(x1, y1)| {
                let tile = &self.tiles[*x1][*y1];

                !tile.is_recycler && tile.scrap > 0 && !visited.contains(&(*x1, *y1))
            }) {
                queue.push_back(*next);
            }
        }

        (visited, zone_owner)
    }

    fn bfs_to(&self, start: Position, end: Position) -> Vec<Position> {
        let mut queue: VecDeque<Position> = VecDeque::new();
        let mut visited: Vec<Option<Position>> = vec![None; self.h * self.w];
        let mut found = false;

        queue.push_back(start);

        'outer: while !queue.is_empty() {
            let (x, y) = queue.pop_front().unwrap();
            let tile = &self.tiles[x][y];

            for next in tile.neighbors.iter().filter(|(x1, y1)| {
                let nei = &self.tiles[*x1][*y1];

                !nei.is_recycler && nei.scrap > 0 && !(nei.is_recycler_near && nei.scrap <= distance(&start, &(*x1, *y1))) && !(nei.owner == Owner::ME && nei.units > tile.units)
            }) {
                let next_tile = &self.tiles[next.0][next.1];

                if next == &end {
                    visited[next.0 + next.1 * self.w] = Some((x, y));
                    found = true;
                    break 'outer;
                }
                if visited[next.0 + next.1 * self.w].is_none() {
                    queue.push_back(*next);
                    visited[next.0 + next.1 * self.w] = Some((x, y));
                }
            }
        }

        if !found {
            return vec![];
        }

        let mut path: Vec<Position> = Vec::new();
        let mut p = end;

        path.push(p);

        while p != start {
            p = visited[p.0 + p.1 * self.w].unwrap();
            path.push(p);
        }

        path.reverse();

        return path;
    }

    fn bfs_until(&self, start: Position, until: fn(&Tile) -> bool) -> Vec<Position> {
        let mut queue: VecDeque<Position> = VecDeque::new();
        let mut visited: Vec<Option<Position>> = vec![None; self.h * self.w];
        let mut end = start;
        let mut found = false;

        queue.push_back(start);

        'outer: while !queue.is_empty() {
            let (x, y) = queue.pop_front().unwrap();
            let tile = &self.tiles[x][y];

            for next in tile.neighbors.iter().filter(|(x1, y1)| {
                let nei = &self.tiles[*x1][*y1];

                !nei.is_recycler && nei.scrap > 0 && !(nei.is_recycler_near && nei.scrap <= distance(&start, &(*x1, *y1))) && !(nei.owner == Owner::ME && nei.units > tile.units)
            }) {
                let next_tile = &self.tiles[next.0][next.1];

                if until(next_tile) {
                    visited[next.0 + next.1 * self.w] = Some((x, y));
                    end = (next.0, next.1);
                    found = true;
                    break 'outer;
                }
                if visited[next.0 + next.1 * self.w].is_none() {
                    queue.push_back(*next);
                    visited[next.0 + next.1 * self.w] = Some((x, y));
                }
            }
        }

        if !found {
            return vec![];
        }

        let mut path: Vec<Position> = Vec::new();
        let mut p = end;

        path.push(p);

        while p != start {
            p = visited[p.0 + p.1 * self.w].unwrap();
            path.push(p);
        }

        path.reverse();

        return path;
    }
}

fn main() {
    // INIT GAME STRUCT
    let mut game = Game::new();
    let mut turn: usize = 0;
    let mut early_recyclers: usize = 0;

    // BFS FUNCTIONS
    let bfs_nearest_neutral = |tile: &Tile| tile.owner == Owner::NEUTRAL;
    let bfs_nearest_not_mine = |tile: &Tile| tile.owner != Owner::ME;
    let bfs_nearest_enemy = |tile: &Tile| tile.owner == Owner::OPP;
    let bfs_nearest_enemy_unit = |tile: &Tile| tile.owner == Owner::OPP && tile.units > 0;

    // MAIN GAME LOOP
    loop {
        // PARSE MATTER VALUES
        let [mut matter, _] = game.parse_matter();

        // PARSE TILES
        let [my_units, my_spawnables, my_buildables] = game.parse_tiles();

        // START TIMER
        let now = Instant::now();

        // SET MY_SIDE VALUE

        // GET WEIGHT MAP
        let weight_map = game.get_weight_map();
        let zone_map = game.get_map_zones();

        // DEBUG ZONE OWNERSHIP
        // for y in 0..game.h {
        //     for x in 0..game.w {
        //         match zone_map[&(x, y)] {
        //             Some((owner, _)) => eprint!("{} ", owner.to_string()),
        //             None => eprint!("  ")
        //         }
        //     }
        //     eprint!("\n");
        // }

        // DEBUG ZONE IDs
        // for y in 0..game.h {
        //     for x in 0..game.w {
        //         match zone_map[&(x, y)] {
        //             Some((_, id)) => eprint!("{:2} ", id),
        //             None => eprint!("   ")
        //         }
        //     }
        //     eprint!("\n");
        // }

        // DEBUG BFS
        // let path = game.bfs((game.w - 1, game.h - 1), (0, 0));
        // for y in 0..game.h {
        //     for x in 0..game.w {
        //         match path.contains(&(x, y)) {
        //             true => eprint!("x "),
        //             false => eprint!(". ")
        //         }
        //     }
        //     eprint!("\n");
        // }

        // INIT THE ACTIONS ARRAY
        let mut actions: Vec<String> = vec![];

        // MOVEMENT MANAGEMENT
        for unit in my_units.into_iter().filter_map(|(x, y)| {
            // FILTER OUT TILES: If it's zone owner is already me
            // NOTE: I do this so i stop moving entities in a zone that is secured entierly
            if zone_map[&(x, y)].is_some() && zone_map[&(x, y)].unwrap().0 == Owner::ME { None } else { Some(&game.tiles[x][y]) }
        }) {
            let path = game.bfs_until((unit.x, unit.y), bfs_nearest_not_mine);

            eprintln!("{:?} going through {:?}", (unit.x, unit.y), path);

            if !path.is_empty() {
                actions.push(format!(
                    "MOVE {} {} {} {} {}",
                    (1).max(unit.units / 2), unit.x, unit.y, path[1].0, path[1].1
                ));
            }
            else {
                let mut filtered = unit
                    .neighbors
                    .iter()
                    .filter_map(|(x1, y1)| {
                        let tile = &game.tiles[*x1][*y1];

                        // JUST MAKING SURE WE DONT GO TO UN-WALKABLE TILES
                        if tile.scrap != 0 && !tile.is_recycler {
                            Some(tile)
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<&Tile>>();

                // SORT NEIGHBORS BY THEIR DISTANCE TO A NEUTRAL/ENEMY TILE
                filtered.sort_by_key(|tile| weight_map[&(tile.x, tile.y)]);

                let path_count = filtered.len();

                if path_count > 0 {
                    // SEND UNITS EQUALY PRIORITIZING WORTHEST NEIGHBOR
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
        }

        // BUILD MANAGEMENT
        let mut sorted_builds = my_buildables.into_iter().map(|(x, y)| (&game.tiles[x][y], weight_map[&(x, y)])).collect::<Vec<(&Tile, i32)>>();
        sorted_builds.sort_by(|(tile_a, _), (tile_b, _)| {
            // SORT BY: Distance to center of the map
            // distance(&(tile_a.x, tile_a.y), &(game.w / 2, game.h / 2)).cmp(&distance(&(tile_b.x, tile_b.y), &(game.w / 2, game.h / 2)))
            if early_recyclers < 3 {
                game.get_recycler_worth((tile_b.x, tile_b.y)).cmp(&game.get_recycler_worth((tile_a.x, tile_a.y)))
            } else {
                distance(&(tile_a.x, tile_a.y), &(game.w / 2, tile_a.y)).cmp(&distance(&(tile_b.x, tile_b.y), &(game.w / 2, tile_b.y)))
            }
        });
        for (tile, _) in sorted_builds {
            if matter < 10 {
                break;
            }
            if early_recyclers < 3 {
                if game.get_recycler_worth((tile.x, tile.y)) > 10 {
                    matter -= 10;
                    early_recyclers += 1;

                    actions.push(format!("BUILD {} {}", tile.x, tile.y));
                }
                break;
            } else {
                let opp_neighbors_cnt = tile
                    .neighbors
                    .iter()
                    .filter_map(|(x1, y1)| {
                        let nei = &game.tiles[*x1][*y1];

                        // CHECK IF NEIGHBOR HAS ENEMY UNITS
                        if nei.scrap > 0 && nei.owner == Owner::OPP && nei.units > 0  {
                            // CHECK IF NEIGHBOR HAS ANY OTHER ENEMY TILE NEXT TO IT
                            // NOTE: This prevents building in my own area when an enemy unit enters it alone
                            if nei.neighbors.iter().filter(|(x2, y2)| game.tiles[*x2][*y2].owner == Owner::OPP).count() > 0 {
                                Some(true)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }).count();

                if opp_neighbors_cnt > 0 {
                    matter -= 10;

                    actions.push(format!("BUILD {} {}", tile.x, tile.y));
                }
            }
        }

        // SPAWN MANAGEMENT
        let mut sorted_spawns = my_spawnables.into_iter().filter_map(|(x, y)| {
            // FILTER OUT TILES: If it's zone owner is already me
            // NOTE: I do this so i dont spawn entities in a zone that is secured entierly
            if zone_map[&(x, y)].is_some() && zone_map[&(x, y)].unwrap().0 == Owner::ME { None } else { Some((&game.tiles[x][y], zone_map[&(x, y)].unwrap_or((Owner::ME, -1)))) }
        }).collect::<Vec<(&Tile, (Owner, i32))>>();
        sorted_spawns.sort_by(|(tile_a, (o_a, _)), (tile_b, (o_b, _))| {
            // SORT BY AREA TYPE: Spawn on hostile terrain first then neutral
            // NOTE: If tile A and B have the same area type, we spawn closest to the enemy tiles

            if o_a == o_b {
                if o_a == &Owner::OPP {
                    let dst_a = game.bfs_until((tile_a.x, tile_a.y), bfs_nearest_enemy_unit).len();
                    let dst_b = game.bfs_until((tile_b.x, tile_b.y), bfs_nearest_enemy_unit).len();

                    if dst_a == 0 {
                        Ordering::Greater
                    } else if dst_b == 0 {
                        Ordering::Less
                    } else {
                        dst_a.cmp(&dst_b)
                    }
                } else {
                    Ordering::Equal
                }
            } else {
                o_a.cmp(o_b)
            }
        });

        for (tile, _) in sorted_spawns {
            if matter < 10 {
                break;
            }

            let nearby_opp_units = tile
                .neighbors
                .iter()
                .filter_map(|(x1, y1)| {
                    if game.tiles[*x1][*y1].units > 0 && game.tiles[*x1][*y1].owner == Owner::OPP {
                        Some(game.tiles[*x1][*y1].units)
                    } else {
                        None
                    }
                });

            let mut to_spawn = 0;

            for unit_count in nearby_opp_units {
                to_spawn += unit_count;
            }

            // eprintln!("calculated {} | max spawnable {} | not more than {} | spawning: {}", to_spawn, matter / 10, (to_spawn - tile.units).max(0), to_spawn.min(matter / 10).min((to_spawn - tile.units).max(0)).max(1));

            to_spawn = to_spawn.min(matter / 10).min((to_spawn - tile.units).max(0)).max(1);

            if to_spawn == 0 {
                continue;
            }

            // let neutral_neighbors_cnt = tile
            //     .neighbors
            //     .iter()
            //     .filter_map(|(x1, y1)| {
            //         if game.tiles[*x1][*y1].scrap > 0 && game.tiles[*x1][*y1].owner == Owner::NEUTRAL {
            //             Some(true)
            //         } else {
            //             None
            //         }
            //     }).count();

            // if neutral_neighbors_cnt > 0 {
            matter -= 10 * to_spawn;

            actions.push(format!("SPAWN {} {} {}", to_spawn, tile.x, tile.y));
            // }
        }

        actions.push(format!("MESSAGE Took {:.3} ms", (now.elapsed().as_micros() as f64 / 1000 as f64)));

        // PRINT ACTION(S)
        if actions.len() > 0 {
            println!("{}", actions.join(";"));
        } else {
            println!("WAIT");
        }

        turn += 1;
    }
}