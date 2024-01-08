use spirv_tools::error::MessageCallback;

pub struct Callback;

impl MessageCallback for Callback {
    fn on_message(&mut self, msg: spirv_tools::error::Message) {
        ()
    }
}
