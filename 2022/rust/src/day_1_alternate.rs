// trait Num<const VALUE: u128> {
//     type Output;
// }

// struct NumWrapper<const VALUE: u128>;

// trait Peano {
//     type Output;
// }

// impl<T: Peano, const VALUE: u128> Peano for NumWrapper<{ VALUE }> {
//     type Output = 1 + T::Output;
// }

// struct Plus<T>(PhantomData<T>);

// struct Zero;

// impl Num<0> for _0 {
//     type Output = Zero;
// }
