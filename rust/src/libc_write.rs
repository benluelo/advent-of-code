use core::fmt::Write;

pub struct Stdout;

impl Write for Stdout {
    #[inline]
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let msg = s.as_bytes();

        let mut written = 0;
        while written < msg.len() {
            let bytes: &[u8] = &msg[written..];

            let res = usize::try_from(unsafe {
                libc::write(1, bytes.as_ptr().cast::<core::ffi::c_void>(), bytes.len())
            });

            match res {
                Ok(res) => written += res,
                // Ignore errors
                Err(_) => break,
            }
        }

        Ok(())
    }
}
