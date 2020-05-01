use rand::prelude::*;

use lazy_static::*;

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! 
{
    static ref EVALUATION_HASHMAP: Mutex<HashMap<HashableBoard, f64>> = Mutex::new(HashMap::new()); 
}

// Evaluate a board
pub fn eval_board(board: &Board) -> f64
{
    let mut map = EVALUATION_HASHMAP.lock().unwrap();
    let h = board.get_bin();

    if !map.contains_key(&h)
    {
        let val = true_eval(board);
        map.insert(h, val);
    }
    
    *map.get(&h).unwrap()
}

// Actual (without the wrapper) evaluation function
pub fn true_eval(board: &Board) -> f64
{
    eval_side(board, Color::White) - eval_side(board, Color::Black)
}

// Evaluation function for individual sides
pub fn eval_side(board: &Board, side: Color) -> f64
{
    let mut b = board.clone();
    b.current_turn = side;
    let mut results = b.get_move_results();
    let mov_count = results.all_moves().len();

    let mut result = (3 * results.check_results.len()) as f64 * 0.1;

    b.cache_attacks();
    result += match side {Color::White => b.attacked_by_white.len(),
                          Color::Black => b.attacked_by_black.len()} as f64 * 0.05;

    let piece_attack_amt = 0.01;

    match side
    {
        Color::White =>
        {
            for pos in &b.attacked_by_white
            {
                match &b.get_cell(*pos)
                {
                    Some(_) => {result += piece_attack_amt},
                    None => {}
                }
            }
        },
        Color::Black =>
        {
            for pos in &b.attacked_by_white
            {
                match &b.get_cell(*pos)
                {
                    Some(_) => {result += piece_attack_amt},
                    None => {}
                }
            }
        }
    }

    if b.check(side)
    {
        result -= 5.0;
    }

    if mov_count == 0 && b.check(side)
    {
        result -= 1000.0;
    }

    result
}

/// Colors
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Color
{
    White,
    Black
}

/// Piece Type
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PieceType
{
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King
}

/// Piece
#[derive(Debug, Clone, Copy)]
pub struct Piece
{
    color: Color,
    piece: PieceType
}

impl Piece
{
    /// Generate a new Piece from a string
    pub fn new(s: &str) -> Self
    {
        Self
        {
            color: match s.chars().nth(0).unwrap() { 'W' => Color::White, 'B'=> Color::Black, _default => Color::Black},
            piece: match s.chars().nth(1).unwrap()
            {
                'K' => PieceType::King,
                'P' => PieceType::Pawn,
                'k' => PieceType::Knight,
                'B' => PieceType::Bishop,
                'Q' => PieceType::Queen,
                'R' => PieceType::Rook,
                _default => PieceType::Pawn
            }
        }
    }

    /// Generate a new Piece from a byte
    pub fn from_byte(val: u8) -> Self
    {
        Self
        {
            color: if val & 0x80 > 0 {Color::Black} else {Color::White},
            piece: match val & 0b111
            {
                1 => PieceType::Pawn,
                2 => PieceType::Knight,
                3 => PieceType::Bishop,
                4 => PieceType::Rook,
                5 => PieceType::Queen,
                6 => PieceType::King,
                _default => PieceType::Pawn
            }
        }
    }

    /// Converts a piece to a byte
    pub fn to_byte(&self) -> u8
    {
        let mut result: u8 = 0;

        if self.color == Color::Black
        {
            result |= 0x80;
        }

        result |= match self.piece
        {
            PieceType::Pawn => 1,
            PieceType::Knight => 2,
            PieceType::Bishop => 3,
            PieceType::Rook => 4,
            PieceType::Queen => 5,
            PieceType::King => 6
        };

        result
    }
}

fn p(s: &str) -> Option<Piece>
{
    if String::from(s) == ""
    {
        None
    }
    else
    {
        Some(Piece::new(s))
    }
}

/// Binary Representation of a Board
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct HashableBoard
{
    set0: u128,
    set1: u128,
    set2: u128,
    set3: u128
}

/// Board
#[derive(Debug, Clone)]
pub struct Board
{
    /// Current Side to Move
    current_turn: Color,

    /// Vector of Piece Options
    data: Vec<Vec<Option<Piece>>>,

    /// Cells under attack
    attacked_by_white: Vec<(usize, usize)>,
    attacked_by_black: Vec<(usize, usize)>,

    /// Have the attacked cells been calculated
    attacks_calculated: bool,

    // Positions of each king
    white_king_pos: (usize, usize),
    black_king_pos: (usize, usize)
}

impl Board
{
    /// Generate a new board
    pub fn new() -> Self
    {
        Self
        {
            current_turn: Color::White,

            data: vec![
                vec![p("WR"), p("Wk"), p("WB"), p("WQ"), p("WK"), p("WB"), p("Wk"), p("WR")],
                vec![p("WP"), p("WP"), p("WP"), p("WP"), p("WP"), p("WP"), p("WP"), p("WP")],
                vec![None, None, None, None, None, None, None, None],
                vec![None, None, None, None, None, None, None, None],
                vec![None, None, None, None, None, None, None, None],
                vec![None, None, None, None, None, None, None, None],
                vec![p("BP"), p("BP"), p("BP"), p("BP"), p("BP"), p("BP"), p("BP"), p("BP")],
                vec![p("BR"), p("Bk"), p("BB"), p("BQ"), p("BK"), p("BB"), p("Bk"), p("BR")],
            ],

            attacked_by_white: vec![],
            attacked_by_black: vec![],

            attacks_calculated: false,

            white_king_pos: (4, 0),
            black_king_pos: (4, 7)
        }
    }

    /// Get Hashable
    pub fn get_bin(&self) -> HashableBoard
    {
        let mut set0 = 0u128;
        let mut set1 = 0u128;
        let mut set2 = 0u128;
        let mut set3 = 0u128;

        for y in 0..2
        {
            for x in 0..8
            {
                set0 += match self.data[y][x] { None=> 0, Some(v) => v.to_byte()} as u128;
                set1 += match self.data[y + 2][x] { None=> 0, Some(v) => v.to_byte()} as u128;
                set2 += match self.data[y + 4][x] { None=> 0, Some(v) => v.to_byte()} as u128;
                set3 += match self.data[y + 6][x] { None=> 0, Some(v) => v.to_byte()} as u128;

                set0 <<= 8;
                set1 <<= 8;
                set2 <<= 8;
                set3 <<= 8;
            }
        }

        HashableBoard { set0, set1, set2, set3}
    }
    
    /// Make move
    pub fn make_move(&mut self, mov: &Move)
    {
        let mut p = self.get_cell(mov.start);

        // Replace Cell with Nothing
        self.data[mov.start.1][mov.start.0] = None;

        // Replace the piece if it is a pawn promotion
        match p
        {
            Some(_) => 
            {
                match mov.replace_piece
                {
                    Some(pt) =>
                    {
                        p.unwrap().piece = pt;
                    },
                    None => {}
                }
            },
            None => {}
        }

        // Move the piece to the new cell
        self.data[mov.end.1][mov.end.0] = p;

        // Handle Castling
        if mov.castle
        {
            if mov.end.0 == 2
            {
                self.data[mov.end.1][3] = self.data[mov.end.1][0];
                self.data[mov.end.1][0] = None;
            }
            else if mov.end.0 == 6
            {
                self.data[mov.end.1][5] = self.data[mov.end.1][7];
                self.data[mov.end.1][7] = None;
            }
        }

        // Switch which side is currently to move
        self.switch_side();

        // Clear the attacked arrays
        self.attacked_by_black = vec![];
        self.attacked_by_white = vec![];
        self.attacks_calculated = false;

        // Move the king positions if necessary
        if mov.start == self.white_king_pos
        {
            self.white_king_pos = mov.end;
        }

        if mov.start == self.black_king_pos
        {
            self.black_king_pos = mov.end;
        }
    }

    /// Switch which side is to move
    pub fn switch_side(&mut self)
    {
        self.current_turn = match self.current_turn { Color::White => Color::Black,
                                                      Color::Black => Color::White};
    }

    /// Does the board put the given side in check?
    pub fn check(&mut self, side: Color) -> bool
    {
        if !self.attacks_calculated
        {
            match side
            {
                Color::White => self.is_attacked_by(self.white_king_pos, Color::Black),
                Color::Black => self.is_attacked_by(self.black_king_pos, Color::White)
            }
        }
        else
        {
            match side
            {
                Color::White => self.attacked_by_black.contains(&self.white_king_pos),
                Color::Black => self.attacked_by_white.contains(&self.black_king_pos)
            }
        }
    }

    /// Get a board if the given move is made
    pub fn board_for_move(&self, mov: &Move) -> Board
    {
        let mut copy = self.clone();
        copy.make_move(mov);

        copy
    }

    /// Get a cell by a tuple index
    pub fn get_cell(&self, pos: (usize, usize)) -> Option<Piece>
    {
        self.data[pos.1][pos.0]
    }

    /// Is a given cell attacked (non-cached, for internal use only)
    fn is_attacked_by(&self, pos: (usize, usize), color: Color) -> bool
    {
        // Check for diagonal attacks
        for dir in vec![(-1, -1), (-1, 1), (1, -1), (1, 1)]
        {
            let mut current_pos = (pos.0 as isize, pos.1 as isize);

            loop
            {
                current_pos = ((current_pos.0 + dir.0),
                                (current_pos.1 + dir.1));

                // Stop when off of the board
                if current_pos.0 < 0 || current_pos.0 > 7 || current_pos.1 < 0 || current_pos.1 > 7
                {
                    break;
                }

                let new_pos = (current_pos.0 as usize, current_pos.1 as usize);

                match self.get_cell(new_pos)
                {
                    None => {},
                    Some(piece) =>
                    {
                        if piece.color == color
                        {
                            if piece.piece == PieceType::Bishop || piece.piece == PieceType::Queen
                            {
                                return true;
                            }
                        }

                        break;
                    }
                }
            }  
        }

        // Check for orthogonal attacks
        for dir in vec![(-1, 0), (1, 0), (0, -1), (0, 1)]
        {
            let mut current_pos = (pos.0 as isize, pos.1 as isize);

            loop
            {
                current_pos = ((current_pos.0 + dir.0),
                                (current_pos.1 + dir.1));

                // Stop when off of the board
                if current_pos.0 < 0 || current_pos.0 > 7 || current_pos.1 < 0 || current_pos.1 > 7
                {
                    break;
                }

                let new_pos = (current_pos.0 as usize, current_pos.1 as usize);

                match self.get_cell(new_pos)
                {
                    None => {},
                    Some(piece) =>
                    {
                        if piece.color == color
                        {
                            if piece.piece == PieceType::Rook || piece.piece == PieceType::Queen
                            {
                                return true;
                            }
                        }

                        break;
                    }
                }
            }  
        }

        // Check for knight attacks
        for (scale_x, scale_y) in vec![(1isize, 1isize), (1, -1), (-1, 1), (-1, -1)]
        {
            for (mut dx, mut dy) in vec![(1isize, 2isize), (2, 1)]
            {
                dx *= scale_x;
                dy *= scale_y;

                if (pos.0 as isize) + dx < 0 || (pos.0 as isize) + dx > 7 ||
                    (pos.1 as isize) + dy < 0 || (pos.1 as isize) + dy > 7 
                {
                    continue;
                }

                let next_pos = ((pos.0 as isize + dx) as usize, 
                                                (pos.1 as isize + dy) as usize);
                
                match self.get_cell(next_pos)
                {
                    None => {},
                    Some(piece) => 
                    {
                        if piece.color == color
                        {
                            if piece.piece == PieceType::Knight
                            {
                                return true;
                            }
                        }
                        break;
                    }
                }
            }
        }

        // Check for pawn attacks
        let new_y = pos.1 as isize + match color {Color::White => -1, Color::Black => 1};
        for dx in vec![-1isize, 1]
        {
            if (pos.0 as isize) + dx < 0 || (pos.0 as isize) + dx > 7 || new_y < 0 || new_y > 7
            {
                continue;
            }

            let next_pos = ((pos.0 as isize + dx) as usize, 
                                            new_y as usize);
            
            match self.get_cell(next_pos)
            {
                None => {},
                Some(piece) => 
                {
                    if piece.color == color
                    {
                        if piece.piece == PieceType::Pawn
                        {
                            return true;
                        }
                    }
                }
            }
        }

        // Check for king attacks
        for dx in vec![-1isize, 0, 1]
        {
            for dy in vec![-1isize, 0, 1]
            {
                if dx == dy && dx == 0
                {
                    continue;
                }

                if (pos.0 as isize) < -dx || (pos.0 as isize) > 7 - dx ||
                    (pos.1 as isize) < -dy || (pos.1 as isize) > 7 - dy
                {
                    continue;
                }

                let next_pos = ((pos.0 as isize + dx) as usize, 
                                                (pos.1 as isize + dy) as usize);

                match self.get_cell(next_pos)
                {
                    None => {},
                    Some(piece) => 
                    {
                        if piece.color == color
                        {
                            if piece.piece == PieceType::King
                            {
                                return true;
                            }
                        }
                    }
                }
            }
        }

        // Otherwise, no attacks
        false
    }

    /// Calculate attacks for all positions on the board
    pub fn cache_attacks(&mut self)
    {
        for x in 0usize..8
        {
            for y in 0usize..8
            {
                if self.is_attacked_by((x, y), Color::White)
                {
                    self.attacked_by_white.push((x, y));
                }

                if self.is_attacked_by((x, y), Color::Black)
                {
                    self.attacked_by_black.push((x, y));
                }
            }
        }

        self.attacks_calculated = true;
    }

    /// Get all of the possible moves from this board position
    pub fn get_move_results(&mut self) -> Results
    {
        let mut result = Results::new();

        for x in 0usize..8
        {
            for y in 0usize..8
            {
                let pos = (x, y);

                match self.get_cell(pos)
                {
                    None => {},
                    Some(piece) => 
                    {
                        // Ignore if a piece isn't of the current player's color
                        if piece.color != self.current_turn
                        {
                            continue;
                        }

                        // Diagonal Moves
                        if piece.piece == PieceType::Bishop ||
                           piece.piece == PieceType::Queen
                        {
                            for dir in vec![(-1, -1), (-1, 1), (1, -1), (1, 1)]
                            {
                                let mut current_pos = (pos.0 as isize, pos.1 as isize);

                                loop
                                {
                                    current_pos = ((current_pos.0 + dir.0),
                                                   (current_pos.1 + dir.1));

                                    // Stop when off of the board
                                    if current_pos.0 < 0 || current_pos.0 > 7 || current_pos.1 < 0 || current_pos.1 > 7
                                    {
                                        break;
                                    }

                                    let new_pos = (current_pos.0 as usize, current_pos.1 as usize);

                                    if self.take_or_move(new_pos)
                                    {
                                        result.add_move(Move::new(pos, new_pos), &self);
                                    }

                                    if self.is_blocked(new_pos)
                                    {
                                        break;
                                    }
                                }
                            }
                        }

                        // Orthogonal Moves
                        if piece.piece == PieceType::Rook ||
                           piece.piece == PieceType::Queen
                        {
                            for dir in vec![(-1, 0), (1, 0), (0, -1), (0, 1)]
                            {
                                let mut current_pos = (pos.0 as isize, pos.1 as isize);

                                loop
                                {
                                    current_pos = ((current_pos.0 + dir.0),
                                                   (current_pos.1 + dir.1));

                                    // Stop when off of the board
                                    if current_pos.0 < 0 || current_pos.0 > 7 || current_pos.1 < 0 || current_pos.1 > 7
                                    {
                                        break;
                                    }

                                    let new_pos = (current_pos.0 as usize, current_pos.1 as usize);

                                    if self.take_or_move(new_pos)
                                    {
                                        result.add_move(Move::new(pos, new_pos), &self);
                                    }

                                    if self.is_blocked(new_pos)
                                    {
                                        break;
                                    }
                                }
                            }
                        }

                        // Knight Moves
                        if piece.piece == PieceType::Knight
                        {
                            for (scale_x, scale_y) in vec![(1isize, 1isize), (1, -1), (-1, 1), (-1, -1)]
                            {
                                for (mut dx, mut dy) in vec![(1isize, 2isize), (2, 1)]
                                {
                                    dx *= scale_x;
                                    dy *= scale_y;

                                    if (pos.0 as isize) < -dx || (pos.0 as isize) > 7 - dx ||
                                       (pos.1 as isize) < -dy || (pos.1 as isize) > 7 - dy
                                    {
                                        continue;
                                    }

                                    let next_pos = ((pos.0 as isize + dx) as usize, 
                                                                    (pos.1 as isize + dy) as usize);
                                    
                                    if self.take_or_move(next_pos)
                                    {
                                        result.add_move(Move::new(pos, next_pos), &self);
                                    }
                                }
                            }
                        }

                        // Pawn Moves
                        if piece.piece == PieceType::Pawn
                        {
                            let dir: isize;
                            let home_row: usize;
                            let promote_row: usize;

                            match self.current_turn
                            {
                                Color::Black =>
                                {
                                    dir = -1;
                                    home_row = 6;
                                    promote_row = 0;
                                },
                                Color::White =>
                                {
                                    dir = 1;
                                    home_row = 1;
                                    promote_row = 7;
                                }
                            }

                            let one_forward = (pos.0, (pos.1 as isize + dir) as usize);
                            let two_forward = (pos.0, (pos.1 as isize + 2 * dir) as usize);

                            let mut move_one = Move::new(pos, one_forward);

                            if !self.is_blocked(one_forward)
                            {
                            
                                // Add promotion handling
                                if one_forward.1 == promote_row
                                {
                                    for piece_type in vec![PieceType::Rook, PieceType::Queen, PieceType::Bishop, PieceType::Knight]
                                    {
                                        move_one.replace_piece = Some(piece_type);
                                        result.add_move(move_one, &self);
                                    }
                                }
                                else
                                {
                                    result.add_move(move_one, &self);
                                }

                                

                                // If on the first move, a pawn can be moved forward two spaces
                                if pos.1 == home_row
                                {
                                    if !self.is_blocked(two_forward)
                                    {
                                        result.add_move(Move::new(pos, two_forward), &self);
                                    }
                                }

                                // Check for an attack on the left
                                if pos.0 > 0
                                {
                                    let left_attack = (pos.0 - 1, one_forward.1);

                                    if self.can_take(left_attack)
                                    {
                                        result.add_move(Move::new(pos, left_attack), &self);
                                    }
                                }

                                // Check for an attack on the right
                                if pos.0 < 7
                                {
                                    let right_attack = (pos.0 + 1, one_forward.1);

                                    if self.can_take(right_attack)
                                    {
                                        result.add_move(Move::new(pos, right_attack), &self);
                                    }
                                }
                            }
                        }

                        // King Moves
                        if piece.piece == PieceType::King
                        {
                            // Normal Moves
                            for dx in vec![-1isize, 0, 1]
                            {
                                for dy in vec![-1isize, 0, 1]
                                {
                                    if dx == dy && dx == 0
                                    {
                                        continue;
                                    }

                                    if (pos.0 as isize) + dx < 0 || (pos.0 as isize) + dx > 7 ||
                                       (pos.1 as isize) + dy < 0 || (pos.1 as isize) + dy > 7
                                    {
                                        continue;
                                    }

                                    let next_pos = ((pos.0 as isize + dx) as usize, 
                                                                    (pos.1 as isize + dy) as usize);

                                    if self.take_or_move(next_pos)
                                    {
                                        result.add_move(Move::new(pos, next_pos), &self);
                                    }
                                }
                            }

                            // Castling
                            let row = match self.current_turn {Color::White => 0, Color::Black => 7};


                            if self.current_turn == Color::White && self.white_king_pos == (4, row) ||
                               self.current_turn == Color::Black && self.black_king_pos == (4, row)
                            {
                                match self.get_cell((0, row))
                                {
                                    None => {},
                                    Some(piece) =>
                                    {
                                        if piece.piece == PieceType::Rook && 
                                           piece.color == self.current_turn &&
                                           !self.is_blocked((1, row)) &&
                                           !self.is_blocked((2, row)) &&
                                           !self.is_blocked((3, row))
                                        {
                                            let mut mov = Move::new(pos, (2, row));
                                            mov.castle = true;
                                            result.add_move(mov, &self);
                                        }
                                    }
                                }

                                match self.get_cell((7, row))
                                {
                                    None => {},
                                    Some(piece) =>
                                    {
                                        if piece.piece == PieceType::Rook && 
                                           piece.color == self.current_turn &&
                                           !self.is_blocked((5, row)) &&
                                           !self.is_blocked((6, row))
                                        {
                                            let mut mov = Move::new(pos, (6, row));
                                            mov.castle = true;
                                            result.add_move(mov, &self);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        result
    }

    // Check if a position is blocked
    pub fn is_blocked(&self, pos: (usize, usize)) -> bool
    {
        match self.get_cell(pos)
        {
            None => false,
            Some(_) => true
        }
    }

    // Check if a position can be taken
    pub fn can_take(&self, pos: (usize, usize)) -> bool
    {
        match self.get_cell(pos)
        {
            None => false,
            Some(piece) => piece.color != self.current_turn
        }
    }

    // Check if a position can be taken or moved to
    pub fn take_or_move(&self, pos: (usize, usize)) -> bool
    {
        match self.get_cell(pos)
        {
            None => true,
            Some(piece) => piece.color != self.current_turn
        }
    }

    // Get best move
    pub fn get_best_move(&mut self) -> Move
    {
        let results = self.get_move_results().all_moves();

        let mut best = (match self.current_turn {Color::White => -10000.0, Color::Black => 10000.0}, Move::new((0, 0), (0, 0)));

        for mov in &results
        {
            let score = self.board_for_move(&mov).min_max(2, -10000000.0, 10000000.0);
            if self.current_turn == Color::White && score > best.0
            {
                best = (score, *mov);
            }

            if self.current_turn == Color::Black && score < best.0
            {
                best = (score, *mov);
            }

        }

        best.1
    }

    // Min Max
    pub fn min_max(&mut self, depth: usize, mut alpha: f64, mut beta: f64) -> f64
    {
        if depth == 0
        {
            eval_board(&self)
        }
        else
        {
            let results = self.get_move_results().all_moves();

            let mut best = (match self.current_turn {Color::White => -10000.0, Color::Black => 10000.0}, Move::new((0, 0), (0, 0)));

            for mov in &results
            {
                let score = self.board_for_move(&mov).min_max(depth-1, alpha, beta);
                if self.current_turn == Color::White
                {
                    if score > best.0
                    {
                        best = (score, *mov);
                    }

                    if score > alpha 
                    {
                        alpha = score;
                    }

                    if beta <= alpha
                    {
                        break;
                    }
                }

                if self.current_turn == Color::Black
                {
                    if score < best.0 
                    {
                        best = (score, *mov);
                    }

                    if score < beta
                    {
                        beta = score;
                    }

                    if beta <= alpha
                    {
                        break;
                    }
                }

            }

            best.0
        }
    }
}

fn convert_pos(val: (usize, usize)) -> String
{
    let mut result = String::new();

    result += match val.0
    {
        0 => "a",
        1 => "b",
        2 => "c",
        3 => "d",
        4 => "e",
        5 => "f",
        6 => "g",
        7 => "h",
        _default => "PANIC!!!"
    };

    result += &format!("{}", val.1 + 1);

    result
}

fn parse_pos(val: String) -> (usize, usize)
{
    (
        match val.chars().nth(0).unwrap()
        {
            'a' => 0,
            'b' => 1,
            'c' => 2,
            'd' => 3,
            'e' => 4,
            'f' => 5,
            'g' => 6,
            'h' => 7,
            _default => 0
        } as usize,
        match val.chars().nth(1).unwrap()
        {
            '1' => 0,
            '2' => 1,
            '3' => 2,
            '4' => 3,
            '5' => 4,
            '6' => 5,
            '7' => 6,
            '8' => 7,
            _default => 0
        } as usize
    )
}

/// Stores information about a given move
#[derive(Debug, Clone, Copy)]
pub struct Move
{
    start: (usize, usize),
    end: (usize, usize),
    replace_piece: Option<PieceType>,
    castle: bool
}

impl Move
{
    /// Generate a new Move Object
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self
    {
        Self
        {
            start: start,
            end: end,
            replace_piece: None,
            castle: false
        }
    }

    /// Parse a move from the input sent over an XBoard Connection
    pub fn parse(s: String) -> Option<Self>
    {
        let mut guess = Self::new(parse_pos(s[0..2].to_string()), parse_pos(s[2..4].to_string()));

        if (guess.start.0 == 4 && (guess.start.1 == 0 || guess.start.1 == 7)) && 
           ((guess.end.0 == 2 || guess.end.0 == 6) && (guess.end.1 == 0 || guess.end.1 == 7))
        {
            guess.castle = true;
        }

        if s.len() > 4
        {
            guess.replace_piece = Some(match s.chars().nth(4).unwrap()
            {
                'k' => PieceType::Knight,
                'b' => PieceType::Bishop,
                'q' => PieceType::Queen,
                'r' => PieceType::Rook,
                _default => PieceType::Pawn
            });
        }

        Some(guess)
    }

    /// Generate a move for an XBoard Connection
    pub fn to_string(&self) -> String
    {
        let mut result = convert_pos(self.start);
        result += &convert_pos(self.end);

        match self.replace_piece
        {
            Some(p) => 
            {
                result += match p
                {
                    PieceType::Rook => "r",
                    PieceType::Queen => "q",
                    PieceType::Knight => "k",
                    PieceType::Bishop => "b",
                    _default => "PANIC!"
                };
            },
            None => {}
        }

        result
    }
}

/// Move Results
#[derive(Debug, Clone)]
pub struct Results
{
    /// Results which put the opponent in check
    check_results: Vec<Move>,
    /// Results which take an opponent piece
    take_results: Vec<Move>,
    /// Results which do none of the above
    other_results: Vec<Move>,
}

impl Results
{
    /// Generates a new, empty Results object
    pub fn new() -> Self
    {
        Self
        {
            check_results: vec![],
            take_results: vec![],
            other_results: vec![]
        }
    }

    /// Add a new move
    pub fn add_move(&mut self, mov: Move, board: &Board)
    {
        let mut post_move_board = board.board_for_move(&mov);
        let end_piece = board.get_cell(mov.end);

        // Make supre the move doesn't put this side in check
        if post_move_board.check(board.current_turn)
        {
            return;
        }

        // Check if the move puts the opponent into check
        if post_move_board.check(post_move_board.current_turn)
        {
            self.check_results.push(mov);
        }
        // Check if the move captures an opposing piece
        else if match end_piece { None => false, Some(_) => true } &&
                end_piece.unwrap().color == post_move_board.current_turn
        {
            self.take_results.push(mov);
        }
        // Any other move
        else
        {
            self.other_results.push(mov);
        }
    }

    /// Get All Moves
    pub fn all_moves(&mut self) -> Vec<Move>
    {
        let mut rng = rand::thread_rng();
        let mut result = Vec::<Move>::new();

        self.check_results.shuffle(&mut rng);
        self.take_results.shuffle(&mut rng);
        self.other_results.shuffle(&mut rng);


        result.append(&mut self.check_results);
        result.append(&mut self.take_results);
        result.append(&mut self.other_results);

        result
    }
}