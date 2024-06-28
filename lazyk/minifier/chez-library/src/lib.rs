use std::cell::RefCell;
use std::ffi;

// `lazyk_minifier_core` gives us a function like:
//
//     fn minify(&[u8]) -> Vec<u8>;
//
// The role of this crate is to adapt this function to make it more friendly
// for Chez Scheme's FFI. We want to be able to use it as follows:
//
//     (load-shared-object "path/to/this-library.dylib")
//     (define minify
//       (foreign-procedure "lazyk_minify" (string) string))
//
// Which corresponds to a C function with a signature like:
//
//     const char *lazyk_minify(const char *);
//
// Here Chez does the work of converting between its own string representation
// and the NUL-terminated arrays expected for C FFI.
//
// The main wrinkle is memory management. Chez Scheme doesn't impose any requirements
// on how the char* returned from the function should be allocated, but it also doesn't
// take responsibility for freeing it. So what we do is keep a global buffer to store the
// result that Gs reused between calls.

thread_local! {
    static OUT_BUFFER: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

#[no_mangle]
pub unsafe extern "C" fn lazyk_minify(ptr: *const ffi::c_char) -> *const ffi::c_char {
    match std::panic::catch_unwind(|| {
        // SAFETY: guarantees this is a NUL-terminated array
        let slice = ffi::CStr::from_ptr(ptr).to_bytes();
        OUT_BUFFER.with_borrow_mut(|buf| {
            buf.clear();
            buf.append(&mut lazyk_minifier_core::minify(slice));
            buf.push(b'\0');
            buf.as_ptr()
        })
    }) {
        // if we panicked on the Rust side, just kill the process
        Err(e) => {
            std::mem::forget(e); // don't call the destructor, since it might in turn panic
            std::process::exit(1)
        }
        // SAFETY: Chez Scheme guarantees that the returned pointer is immediately copied
        // to create a Scheme string. So while this pointer points to an unlocked RefCell,
        // it can't become invalidated until after it will no longer be accessed.
        Ok(ptr) => ptr as *const ffi::c_char,
    }
}
