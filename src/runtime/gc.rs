use std::num::Wrapping;
use std::cell::Cell;
use std::ptr::NonNull;
use std::ops::{Deref, DerefMut};
use std::clone::Clone;
use std::iter::Iterator;
use crate::runtime::object::Object;

trait Root {
    fn gc_box(&self) -> &GcBox;
}

pub struct GcBox {
    strong: Cell<usize>,
    mark: Cell<usize>,
    inner: Object,
}

impl GcBox {
    fn inc_strong(&self) {
        self.strong.set(self.strong.get() + 1)
    }

    fn dec_strong(&self) {
        self.strong.set(self.strong.get() - 1)
    }

    fn handle(&mut self) -> GcHandle {
        self.inc_strong();

        GcHandle {
            inner: NonNull::new(self).unwrap()
        }
    }
}

#[derive(Clone)]
pub struct Gc {
    inner: NonNull<GcBox>
}

impl Deref for Gc {
    type Target = Object;

    fn deref(&self) -> &Object {
        unsafe { &self.inner.as_ref().inner }
    }
}

impl DerefMut for Gc {
    fn deref_mut(&mut self) -> &mut Object {
        unsafe { &mut self.inner.as_mut().inner }
    }
}

impl Root for Gc {
    fn gc_box(&self) -> &GcBox {
        unsafe { self.inner.as_ref() }
    }
}

impl Root for &Gc {
    fn gc_box(&self) -> &GcBox {
        unsafe { self.inner.as_ref() }
    }
}

pub struct GcHandle {
    inner: NonNull<GcBox>
}

impl GcHandle {
    pub fn as_gc(&self) -> Gc {
        Gc {
            inner: self.inner
        }
    }
}

impl Clone for GcHandle {
    fn clone(&self) -> GcHandle {
        unsafe {
            self.inner.as_ref().inc_strong();
        }

        GcHandle {
            inner: self.inner
        }
    }
}

impl Drop for GcHandle {
    fn drop(&mut self) {
        unsafe {
            self.inner.as_ref().dec_strong();
        }
    }
}

impl Deref for GcHandle {
    type Target = Object;

    fn deref(&self) -> &Object {
        unsafe { &self.inner.as_ref().inner }
    }
}

impl DerefMut for GcHandle {
    fn deref_mut(&mut self) -> &mut Object {
        unsafe { &mut self.inner.as_mut().inner }
    }
}

impl Root for GcHandle {
    fn gc_box(&self) -> &GcBox {
        unsafe { self.inner.as_ref() }
    }
}

impl Root for &GcHandle {
    fn gc_box(&self) -> &GcBox {
        unsafe { self.inner.as_ref() }
    }
}

pub struct Heap {
    pub heap: Vec<Box<GcBox>>,
    round: Wrapping<usize>
}

impl Heap {
    pub fn new() -> Self {
        Heap {
            heap: Vec::new(),
            round: Wrapping(0)
        }
    }

    pub fn construct(&mut self, object: Object) -> GcHandle {
        self.heap.push(Box::new(GcBox {
            strong: Cell::new(0),
            mark: Cell::new(self.round.0),
            inner: object
        }));

        self.heap.last_mut().unwrap().handle()
    }

    pub fn gc<I, T>(&mut self, roots: I)
    where I: Iterator<Item = T>, T: Root {
        self.round += Wrapping(1);
        let round = self.round.0;

        for object in roots {
            object.gc_box().mark.set(round);
        }

        let mut i: usize = 0;
        while i < self.heap.len() {
            let object = &self.heap[i];
            if object.strong.get() == 0 && object.mark.get() != round {
                // No references to this object, so delete it
                self.heap.swap_remove(i);
            } else {
                i += 1;
            }
        }
    }
}
