pub fn solution(input: String) -> u32 {
    let lines = input.lines().map(TermOutputLine::from_str);

    let ft = FileTree::from_term_output_lines(lines);

    let mut total = 0_u32;

    ft.post_order(&mut |dir| {
        let size = dir.size();
        if size <= 100_000 {
            total += size
        }
    });

    total
}

#[derive(Debug)]
enum FileTree<'a> {
    Dir(Dir<'a>),
    File(File<'a>),
}

impl<'a> FileTree<'a> {
    fn from_term_output_lines<'b>(
        mut lines: impl Iterator<Item = TermOutputLine<'b>>,
    ) -> FileTree<'b> {
        fn from_term_output_lines_inner<'inner>(
            parent_dir: &'inner str,
            lines: &mut impl Iterator<Item = TermOutputLine<'inner>>,
        ) -> FileTree<'inner> {
            let mut trees = vec![];

            while let Some(line) = lines.next() {
                match line {
                    TermOutputLine::CdToParent => return FileTree::Dir(Dir(parent_dir, trees)),
                    TermOutputLine::CdToDir(dir) => {
                        trees.push(from_term_output_lines_inner(dir, lines))
                    }
                    TermOutputLine::File(size, file) => {
                        trees.push(FileTree::File(File(size, file)))
                    }
                    // these can be ignored since they don't provide any new information
                    TermOutputLine::Ls => {}
                    TermOutputLine::Dir(_) => {}
                }
            }

            return FileTree::Dir(Dir(parent_dir, trees));
        }

        let TermOutputLine::CdToDir(dir) = lines.next().unwrap() else { panic!() };

        from_term_output_lines_inner(dir, &mut lines)
    }

    fn size(&self) -> u32 {
        match self {
            FileTree::Dir(dir) => dir.size(),
            FileTree::File(file) => file.size(),
        }
    }

    fn post_order<'b>(&'b self, f: &mut impl FnMut(&'b Dir<'a>)) {
        match self {
            FileTree::Dir(dir) => {
                f(dir);

                for ft in dir.1.iter() {
                    ft.post_order(f)
                }
            }
            FileTree::File(File(_, _)) => {}
        }
    }
}

#[derive(Debug)]
struct Dir<'a>(&'a str, Vec<FileTree<'a>>);

impl<'a> Dir<'a> {
    fn size(&self) -> u32 {
        self.1.iter().map(FileTree::size).sum()
    }
}

#[derive(Debug)]
struct File<'a>(&'a str, u32);

impl<'a> File<'a> {
    fn size(&self) -> u32 {
        self.1
    }
}

#[derive(Debug)]
enum TermOutputLine<'a> {
    CdToParent,
    // CdHome,
    CdToDir(&'a str),
    Ls,
    Dir(&'a str),
    File(&'a str, u32),
}

impl TermOutputLine<'_> {
    fn from_str(s: &str) -> TermOutputLine<'_> {
        match s.split_once(' ').unwrap() {
            ("$", "ls") => TermOutputLine::Ls,
            ("$", cd) => match cd.split_once(' ').unwrap() {
                ("cd", "..") => TermOutputLine::CdToParent,
                // ("cd", "/") => TermOutputLine::CdHome,
                ("cd", dir) if !dir.contains(' ') => TermOutputLine::CdToDir(dir),
                _ => panic!("bad input"),
            },
            ("dir", dir) => TermOutputLine::Dir(dir),
            (size, file) if !file.contains(' ') => {
                TermOutputLine::File(file, size.parse().unwrap())
            }
            _ => panic!("bad input"),
        }
    }
}
