pub fn solution(input: &str) -> u32 {
    let ft = FileTree::parse(input);

    let mut total = 0_u32;

    ft.post_order(&mut |dir| {
        let size = dir.size();
        if size <= 100_000 {
            total += size;
        }
    });

    total
}

pub fn solution_part_2(input: &str) -> u32 {
    const DISK_SIZE: u32 = 70_000_000;
    const REQUIRED_SPACE_FOR_UPDATE: u32 = 30_000_000;

    let root = FileTree::parse(input);

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

#[derive(Debug)]
enum FileTree<'a> {
    Dir(Dir<'a>),
    File(File<'a>),
}

impl<'a> FileTree<'a> {
    fn parse(input: &str) -> FileTree<'_> {
        fn from_term_output_lines<'inner>(
            parent_dir: &'inner str,
            lines: &mut impl Iterator<Item = TermOutputLine<'inner>>,
        ) -> FileTree<'inner> {
            let mut trees = vec![];

            while let Some(line) = lines.next() {
                match line {
                    TermOutputLine::CdToParent => return FileTree::Dir(Dir(parent_dir, trees)),
                    TermOutputLine::CdToDir(dir) => trees.push(from_term_output_lines(dir, lines)),
                    TermOutputLine::File(size, file) => {
                        trees.push(FileTree::File(File(size, file)));
                    }
                    // these can be ignored since they don't provide any new information
                    TermOutputLine::Ls | TermOutputLine::Dir(_) => {}
                }
            }

            return FileTree::Dir(Dir(parent_dir, trees));
        }

        let mut lines = input.lines().map(TermOutputLine::from_str);

        let TermOutputLine::CdToDir(dir @ "/") = lines.next().unwrap() else { panic!("missing root directory") };

        from_term_output_lines(dir, &mut lines)
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

                for ft in &dir.1 {
                    ft.post_order(f);
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
                // treat "/" like a normal dir
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
