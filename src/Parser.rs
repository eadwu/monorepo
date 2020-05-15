use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct BoxpubFlags {
    #[structopt(long, default_value = "0")]
    start: u32,
    #[structopt(long, default_value = "0")]
    end: u32,
    #[structopt(long, default_value = "boxnovel")]
    source: String,
    #[structopt(parse(from_os_str), long)]
    output_directory: Option<std::path::PathBuf>,
}

#[derive(Debug, StructOpt)]
pub struct BoxpubOptions {
    #[structopt(short, long)]
    version: bool,
    #[structopt(short, long)]
    verbose: bool,

    #[structopt(flatten)]
    boxpub_flags: BoxpubFlags,

    novel: String,
}
