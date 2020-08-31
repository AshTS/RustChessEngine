#![allow(dead_code)]

pub mod board;
use board::*;

/// States for the Chess Engine Input System (XBoard)
#[derive(Debug, Clone, Copy)]
pub enum EngineState
{
    IdleStart,
    Observing,
    Waiting,
    Thinking,
    Analyzing,
    AnalysisComplete,
    Pondering,
    PonderComplete,
    Closed
}

/// Chess Engine Flags
#[derive(Debug, Clone, Copy)]
pub struct EngineFlags
{
    /// Send Thinking Output
    post: bool,
    /// Pondering
    ponder: bool,
    /// Force Mode
    force: bool,
    /// Random value added to evaluations
    random: bool
}

impl EngineFlags
{
    /// Generate the default EngineFlags object
    pub fn new() -> Self
    {
        Self
        {
            post: false,
            ponder: false,
            force: false,
            random: false
        }
    }
}

/// Chess Engine
#[derive(Debug, Clone)]
pub struct Engine
{
    /// Is the engine still running
    pub running: bool,
    /// Current State
    state: EngineState,
    /// Current Board State
    board: Board,
    /// Engine Flags
    flags: EngineFlags
}

impl Engine
{
    /// Generate a new Engine object
    pub fn new() -> Self
    {
        Self
        {
            running: true,
            state: EngineState::IdleStart,
            board: Board::new(),
            flags: EngineFlags::new()
        }
    }

    /// Set Features
    pub fn set_features(&self)
    {
        println!("tellics say     RustChess v0.1.0");
        println!("tellics say     Carter Plasek");
        println!("feature myname=\"RustChess v0.1.0\"");
        println!("feature ping=1 debug=1 sigint=0 done=0 sigterm=0 sigkill=0");
        println!("feature done=1");
    }

    /// Report Input Event
    pub fn report_input(&mut self, s: String)
    {
        if s == "quit\n"
        {
            self.running = false;
        }
        else if s == "hard\n"
        {
            self.flags.ponder = true;
        }
        else if s == "easy\n"
        {
            self.flags.ponder = false;
        }
        else if s == "post\n"
        {
            self.flags.post = true;
        }
        else if s == "nopost\n"
        {
            self.flags.post = false;
        }
        else if s == "force\n"
        {
            self.flags.force = true;
        }
        else if s == "go\n"
        {
            self.flags.force = false;
        }
        else if s == "random\n"
        {
            self.flags.random = true;
        }
        else if s.starts_with("level")
        {
            // TODO: Add Handling of the level command
        }
        else if s.starts_with("time")
        {
            // TODO: Add Timing
        }
        else if s.starts_with("otim")
        {
            // TODO: Add Timing
        }
        else if s.starts_with("ping ")
        {
            match String::from(&s[5..s.len() - 1]).parse::<usize>()
            {
                Ok(v) =>
                {
                    println!("pong {}", v);
                },
                Err(e) => {panic!("{}", e)}
            }
        }
        else if s.starts_with("protover ")
        {
            match String::from(&s[9..s.len() - 1]).parse::<usize>()
            {
                Ok(v) =>
                {
                    if v < 2
                    {
                        panic!("Does not support a version of XBoard before 2");
                    }
                },
                Err(e) => { panic!("{}", e);}
            }

            self.set_features();
        }
        else
        {
            match self.state
            {
                EngineState::IdleStart =>
                {
                    if s == "xboard\n"
                    {
                        self.state = EngineState::Observing;
                    }
                },
                EngineState::Observing =>
                {
                    if s == "new\n"
                    {
                        self.board = Board::new();
                        self.flags = EngineFlags::new();

                        self.state = EngineState::Waiting;
                    }
                },
                EngineState::Waiting =>
                {
                    // Add the move parsing code here next
                    let m = Move::parse(s[0..s.len() - 1].to_string());
                    match m
                    {
                        Some(mov) => { self.report_move(mov); },
                        None => { }
                    }
                }
                _default => {}
            }
        }
    }

    /// Report a move made by the client
    pub fn report_move(&mut self, mov: Move)
    {
        self.board.make_move(&mov);

         // let my_mov = self.board.get_move_results().all_moves()[0];
        let my_mov = self.board.get_best_move();

        self.make_move(my_mov);
    }

    /// Make a move and send it to the client
    pub fn make_move(&mut self, mov: Move)
    {
        self.board.make_move(&mov);

        println!("move {}", mov.to_string());
    }
}