mod mercenary;

use clap::Parser;
use mercenary::Mercenary;

#[derive(Parser, Debug)]
struct CommandLine {
    /// Prettify log output
    #[clap(short, long, default_value_t = false)]
    pretty: bool,

    /// NATS server address
    nats_address: String,
}

#[tokio::main]
async fn main() -> Result<(), async_nats::Error> {
    let args = CommandLine::parse();

    {
        let logger = tracing_subscriber::fmt().with_line_number(true);

        if args.pretty {
            logger.pretty().init();
        } else {
            logger.init();
        }
    }

    let nc = async_nats::connect(&args.nats_address).await?;
    tracing::info!("Connection established with guild `{}`", &args.nats_address);
    Mercenary::new(nc).routine().await
}
