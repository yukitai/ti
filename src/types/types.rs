use std::rc::Rc;

static mut SYM_ID: usize = 0;

#[derive(Debug)]
pub struct TiSymbol {
    name: Option<Rc<String>>,
    id: usize,
}

impl PartialEq for TiSymbol {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TiSymbol {}

impl Clone for TiSymbol {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            id: self.id,
        }
    }
}

impl TiSymbol {
    pub fn new() -> Self {
        Self {
            name: None,
            id: unsafe { SYM_ID },
        }
    }

    pub fn named(name: Rc<String>) -> Self {
        unsafe {
            SYM_ID += 1;
        }
        Self {
            name: Some(name),
            id: unsafe { SYM_ID },
        }
    }
}

#[derive(Debug, Clone)]
pub enum TiType {
    Num,
    Str,
    Bool,
    TVar(TiSymbol, Option<usize>), // (_, target_in_union_map)
    List(Box<TiType>),
    Map(Box<TiType>, Box<TiType>),
    Mixed(Rc<Vec<(Rc<String>, Rc<TiType>)>>),
}

impl TiType {
    pub fn tvar() -> Self {
        Self::TVar(TiSymbol::new(), None)
    }
}
