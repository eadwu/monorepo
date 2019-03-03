use std::ffi::CStr;
use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn print_string(x: *const c_char) {
  unsafe {
    let cstring = CStr::from_ptr(x);

    if let Ok(input) = cstring.to_str() {
      println!("{}", input);
    } else {
      panic!("Unable to print input");
    }
  }
}
