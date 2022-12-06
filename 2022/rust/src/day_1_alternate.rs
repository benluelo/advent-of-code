use std::marker::PhantomData;

struct NumWrapper<const VALUE: u128>;

trait Peano {
    type Value;
}

trait IsTrue {}
struct Assert<const CHECK: bool>;
impl IsTrue for Assert<true> {}

impl<const VALUE: u128> Peano for NumWrapper<{ VALUE }>
where
    Assert<{ VALUE > 0 }>: IsTrue,
    NumWrapper<{ VALUE - 1 }>: Peano,
{
    type Value = Plus<<NumWrapper<{ VALUE - 1 }> as Peano>::Value>;
}

impl Peano for NumWrapper<0> {
    type Value = Zero;
}

trait Value {
    fn value() -> u32;
}

struct Plus<T>(PhantomData<T>);

impl<T: Value> Value for Plus<T> {
    fn value() -> u32 {
        1 + T::value()
    }
}

impl<T> Plus<T> {
    const fn new() -> Self {
        Self(PhantomData)
    }
}

struct Zero;

impl Value for Zero {
    fn value() -> u32 {
        0
    }
}

// type _5 = <NumWrapper<5> as Peano>::Value;

trait Max<Rhs> {
    type Max;
}

impl<T> Max<Zero> for Plus<T> {
    type Max = Plus<T>;
}

impl<T: Max<U>, U> Max<Plus<T>> for Plus<U> {
    type Max = Plus<<T as Max<U>>::Max>;
}

impl<T> Max<Plus<T>> for Zero {
    type Max = Plus<T>;
}

impl Max<Zero> for Zero {
    type Max = Zero;
}

// fn take_5(_t: <NumWrapper<5> as Peano>::Value) {}

// fn top_level_fns_dont_exist() {
//     take_5();
// }

trait Add<Rhs> {
    type Sum;
}

impl Add<Zero> for Zero {
    type Sum = Zero;
}

impl<T> Add<Zero> for Plus<T> {
    type Sum = Plus<T>;
}

impl<T> Add<Plus<T>> for Zero {
    type Sum = Plus<T>;
}

impl<T: Add<U>, U> Add<Plus<U>> for Plus<T> {
    type Sum = Plus<Plus<<T as Add<U>>::Sum>>;
}

type SumOf<A, B> = <A as Add<B>>::Sum;
type MaxOf<A, B> = <A as Max<B>>::Max;
type ValueOf<T> = <T as Peano>::Value;

const FIVE:
    <<<NumWrapper<3> as Peano>::Value as Max<<NumWrapper<2> as Peano>::Value>>::Max as Max<
        <<NumWrapper<3> as Peano>::Value as Max<<NumWrapper<5> as Peano>::Value>>::Max,
    >>::Max = <Plus<Plus<Plus<Plus<Plus<Zero>>>>>>::new();

macro_rules! build_type {
    (
        $(
            [$($cals:tt,)+]
        )+
    ) => {
        max_of! {
            $(
                sum_of! {
                    $(
                        ValueOf<NumWrapper<$cals>>,
                    )+
                },
            )+
        }
    };
}

macro_rules! max_of {
    ($a:ty, $($tail:tt)+) => {
        MaxOf<$a, max_of!($($tail)+)>
    };

    ($a:ty,) => {
        $a
    };
}

macro_rules! sum_of {
    ($cals:ty, $($cals_tail:tt)+) => {
        SumOf<$cals, sum_of!($($cals_tail)+)>
    };

    ($cals:ty,) => {
        $cals
    };
}

type _1 = ValueOf<NumWrapper<1>>;
type _2 = ValueOf<NumWrapper<2>>;
type _3 = ValueOf<NumWrapper<3>>;

type Summed = sum_of! { _1, _2, _3, _3, };

type Final = build_type! {
    [
        37,
        47,
        30,
    ][
        18,
        21,
        97,
        16,
        23,
        2,
        73,
    ][
        4,
    ][
        36,
        79,
        49,
    ][
        25,
    ]
};

#[test]
fn test_final() {
    println!("{}", <Final as Value>::value());

    let _: Final = ();
}
