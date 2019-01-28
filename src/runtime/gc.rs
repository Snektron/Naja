use std::ptr::NonNull;
use std::cell::Cell;
use std::num::Wrapping;
use std::borrow::{Borrow, BorrowMut};
use std::ops::{Deref, DerefMut};

pub trait Trace {
    fn trace(&self, mark: usize);
}

struct GcBox<T>
where T: Trace + ?Sized {
    rooted: Cell<bool>,
    mark: Cell<usize>,
    data: T
}

impl<T> Trace for GcBox<T>
where T: Trace {
    fn trace(&self, mark: usize) {
        if self.mark.get() != mark {
            self.mark.set(mark);
            self.data.trace(mark);
        }
    }
}

pub struct Gc<T>
where T: Trace {
    ptr: NonNull<GcBox<T>>
}

impl<T> Gc<T>
where T: Trace {
    pub fn try_root(&self) -> Option<Root<T>> {
        let gcbox = unsafe {
            self.ptr.as_ref()
        };

        if gcbox.rooted.get() {
            None
        } else {
            gcbox.rooted.set(true);
            Some(Root {
                ptr: self.ptr
            })
        }
    }

    pub fn root(&self) -> Root<T> {
        self.try_root().expect("Object already rooted")
    }
}

impl<T> Trace for Gc<T>
where T: Trace {
    fn trace(&self, mark: usize) {
        unsafe {
            self.ptr.as_ref().trace(mark);
        }
    }
}

pub struct Root<T>
where T: Trace {
    ptr: NonNull<GcBox<T>>
}

impl<T> Root<T>
where T: Trace {
    pub fn decay(&self) -> Gc<T> {
        Gc {
            ptr: self.ptr
        }
    }

    pub fn unroot(self) {
        // Drops self, unrooting the item
    }
}

impl<T> Drop for Root<T>
where T: Trace {
    fn drop(&mut self) {
        unsafe {
            self.ptr.as_ref().rooted.set(false)
        }
    }
}

impl<T> Borrow<T> for Root<T>
where T: Trace {
    fn borrow(&self) -> &T {
        unsafe {
            &self.ptr.as_ref().data
        }
    }
}

impl<T> BorrowMut<T> for Root<T>
where T: Trace {
    fn borrow_mut(&mut self) -> &mut T {
        unsafe {
            &mut self.ptr.as_mut().data
        }
    }
}

impl<T> Deref for Root<T>
where T: Trace {
    type Target = T;

    fn deref(&self) -> &T {
        self.borrow()
    }
}

impl<T> DerefMut for Root<T>
where T: Trace {
    fn deref_mut(&mut self) -> &mut T {
        self.borrow_mut()
    }
}

struct Environment<'a> {
    objects: Vec<Box<GcBox<dyn Trace + 'a>>>,
    mark: Wrapping<usize>
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            objects: Vec::new(),
            mark: Wrapping(0)
        }
    }

    pub fn construct<T: 'a>(&'a mut self, value: T) -> Root<T> 
    where T: Trace {
        let mut value = Box::new(GcBox {
            rooted: Cell::new(true),
            mark: Cell::new(self.mark.0),
            data: value
        });

        let root = Root {
            ptr: NonNull::new(&mut *value).unwrap()
        };

        self.objects.push(value);
        root
    }

    pub fn gc<T>(&mut self, root: &T)
    where T: Trace {
        self.mark += Wrapping(1);
        root.trace(self.mark.0);
        self.sweep();
    }

    fn sweep(&mut self) {
        let mut i: usize = 0;
        while i < self.objects.len() {
            let object = &self.objects[i];
            if !object.rooted.get() || object.mark.get() != self.mark.0 {
                self.objects.swap_remove(i);
            } else {
                i += 1;
            }
        }
    }
}