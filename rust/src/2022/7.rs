use alloc::{vec, vec::Vec};

use cfg_proc::apply;

use crate::{Day, day, utils::utf8};

#[apply(day)]
impl Day<2022, 7> {
    pub fn parse(input: &[u8]) -> u32 {
        let ft = FileTree::parse(utf8(input));

        let mut total = 0_u32;

        ft.post_order(&mut |dir| {
            let size = dir.size();
            if size <= 100_000 {
                total += size;
            }
        });

        total
    }

    pub fn parse2(input: &[u8]) -> u32 {
        const DISK_SIZE: u32 = 70_000_000;
        const REQUIRED_SPACE_FOR_UPDATE: u32 = 30_000_000;

        let root = FileTree::parse(utf8(input));

        let space_necessary = REQUIRED_SPACE_FOR_UPDATE - (DISK_SIZE - root.size());

        let mut smallest_viable = None;

        root.post_order(&mut |dir| {
            let size = dir.size();
            if size > space_necessary {
                match smallest_viable.as_mut() {
                    Some(sv) => {
                        if size < *sv {
                            *sv = size;
                        }
                    }
                    None => smallest_viable = Some(size),
                }
            }
        });

        smallest_viable.unwrap()
    }
}

#[derive(Debug)]
enum FileTree {
    Dir(Dir),
    File(File),
}

impl FileTree {
    fn parse(input: &str) -> FileTree {
        fn from_term_output_lines<'inner>(
            lines: &mut impl Iterator<Item = TermOutputLine<'inner>>,
        ) -> FileTree {
            let mut trees = vec![];

            while let Some(line) = lines.next() {
                match line {
                    TermOutputLine::CdToParent => return FileTree::Dir(Dir(trees)),
                    TermOutputLine::CdToDir(_dir) => trees.push(from_term_output_lines(lines)),
                    TermOutputLine::File(file) => {
                        trees.push(FileTree::File(File(file)));
                    }
                    // these can be ignored since they don't provide any new information
                    TermOutputLine::Ls | TermOutputLine::Dir(_) => {}
                }
            }

            FileTree::Dir(Dir(trees))
        }

        let mut lines = input.lines().map(TermOutputLine::from_str);

        let TermOutputLine::CdToDir("/") = lines.next().unwrap() else {
            panic!("missing root directory")
        };

        from_term_output_lines(&mut lines)
    }

    fn size(&self) -> u32 {
        match self {
            FileTree::Dir(dir) => dir.size(),
            FileTree::File(file) => file.size(),
        }
    }

    fn post_order<'b>(&'b self, f: &mut impl FnMut(&'b Dir)) {
        match self {
            FileTree::Dir(dir) => {
                f(dir);

                for ft in &dir.0 {
                    ft.post_order(f);
                }
            }
            FileTree::File(File(_)) => {}
        }
    }
}

#[derive(Debug)]
struct Dir(Vec<FileTree>);

impl Dir {
    fn size(&self) -> u32 {
        self.0.iter().map(FileTree::size).sum()
    }
}

#[derive(Debug)]
struct File(u32);

impl File {
    fn size(&self) -> u32 {
        self.0
    }
}

#[derive(Debug)]
enum TermOutputLine<'a> {
    CdToParent,
    CdToDir(&'a str),
    Ls,
    Dir(#[allow(unused)] &'a str),
    File(u32),
}

impl TermOutputLine<'_> {
    fn from_str(s: &str) -> TermOutputLine<'_> {
        match s.split_once(' ').unwrap() {
            ("$", "ls") => TermOutputLine::Ls,
            ("$", cd) => match cd.split_once(' ').unwrap() {
                ("cd", "..") => TermOutputLine::CdToParent,
                // treat "/" like a normal dir
                ("cd", dir) if !dir.contains(' ') => TermOutputLine::CdToDir(dir),
                _ => panic!("bad input"),
            },
            ("dir", dir) => TermOutputLine::Dir(dir),
            (size, file) if !file.contains(' ') => TermOutputLine::File(size.parse().unwrap()),
            _ => panic!("bad input"),
        }
    }
}
