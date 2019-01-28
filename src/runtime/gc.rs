use std::ptr::NonNull;
use std::cell::Cell;
use std::borrow::{Borrow, BorrowMut};
use std::ops::{Deref, DerefMut};

static ROOTED_MASK: u64 = 1 << 63;
static MARK_MASK: u64 = !ROOTED_MASK;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Mark(u64);

impl Mark {
    fn new() -> Self {
        Mark(0)
    }

    fn inc(&mut self) {
        self.0 = (self.0 + 1) & MARK_MASK;
    }
}

#[derive(Clone, Copy)]
struct GcBoxMeta {
    bits: u64
}

impl GcBoxMeta {
    fn new() -> Self {
        GcBoxMeta {
            bits: 0
        }
    }

    fn is_rooted(&self) -> bool {
        self.bits & ROOTED_MASK > 0
    }

    fn set_rooted(&mut self, rooted: bool) {
        if rooted {
            self.bits |= ROOTED_MASK;
        } else {
            self.bits &= !ROOTED_MASK;
        }
    }

    fn mark(&self) -> Mark {
        Mark(self.bits & MARK_MASK)
    }

    fn set_mark(&mut self, mark: Mark) {
        self.bits = (self.bits & ROOTED_MASK) | mark.0;
    }
}

pub trait Trace {
    fn trace(&self, mark: Mark);
}

struct GcBox<T>
where T: Trace + ?Sized {
    meta: Cell<GcBoxMeta>,
    data: T
}

impl<T> Trace for GcBox<T>
where T: Trace {
    fn trace(&self, mark: Mark) {
        let mut meta = self.meta.get();

        if meta.mark() != mark {
            meta.set_mark(mark);
            self.meta.set(meta);
            self.data.trace(mark);
        }
    }
}

#[derive(Clone)]
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

        let mut meta = gcbox.meta.get();
        if meta.is_rooted() {
            None
        } else {
            meta.set_rooted(true);
            gcbox.meta.set(meta);

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
    fn trace(&self, mark: Mark) {
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
        let gcbox = unsafe {
            self.ptr.as_ref()
        };

        let mut meta = gcbox.meta.get();
        meta.set_rooted(false);
        gcbox.meta.set(meta);
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
    mark: Mark
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Environment {
            objects: Vec::new(),
            mark: Mark::new()
        }
    }

    pub fn construct<T>(&'a mut self, value: T) -> Root<T> 
    where T: 'a + Trace {
        let mut value = Box::new(GcBox {
            meta: Cell::new(GcBoxMeta::new()),
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
        self.mark.inc();
        root.trace(self.mark);
        self.sweep();
    }

    fn sweep(&mut self) {
        let mut i: usize = 0;
        while i < self.objects.len() {
            let meta = self.objects[i].meta.get();
            if !meta.is_rooted() && meta.mark() != self.mark {
                self.objects.swap_remove(i);
            } else {
                i += 1;
            }
        }
    }
}