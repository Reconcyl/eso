use std::ops::Index;

#[derive(Debug, Clone, Copy)]
enum Command {
    Change(i8),
    Left(usize),
    Right(usize),
    Up(usize),
    Down(usize),
    
    Jump(BlockIdx),
    CondJump(BlockIdx),
    Call(BlockIdx),
    Ret,
}

#[derive(Debug, Clone)]
struct Block(Vec<Command>);
#[derive(Debug, Clone, Copy)]
struct BlockIdx(usize);

impl Index<BlockIdx> for Block {
    type Output = Command;
    fn index(&self, idx: BlockIdx) -> &Command {
        self.0[idx.0]
    }
}

struct ParseResult {
    blocks: Vec<Block>
}

fn parse(code: &str) -> ParseResult {
    unimplemented!()
}