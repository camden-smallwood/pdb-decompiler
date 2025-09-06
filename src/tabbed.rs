use std::fmt::Display;

pub trait TabbedDisplay {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result;
}

impl<T: Display> TabbedDisplay for T {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (0..depth).map(|_| "    ").collect::<String>().fmt(f)?;
        self.fmt(f)
    }
}

pub struct TabbedDisplayer<'a, T: TabbedDisplay>(pub usize, pub &'a T);

impl<T: TabbedDisplay> Display for TabbedDisplayer<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.tabbed_fmt(self.0, f)
    }
}
