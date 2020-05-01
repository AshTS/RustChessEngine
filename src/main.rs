extern crate lazy_static;
extern crate rand;

mod engine;
use engine::*;

fn main() 
{
    let test_mode = false;

    if !test_mode
    {
        let mut active_engine = Engine::new();

        while active_engine.running
        {
            let mut s = String::new();
            std::io::stdin().read_line(&mut s).unwrap();

            active_engine.report_input(s);
        }
    }
    else
    {
        let p = engine::board::Piece::new("BK");
        println!("{}", p.to_byte());
        println!("{:?}", p);
        println!("{:?}", engine::board::Piece::from_byte(p.to_byte()));
    }
    
}