use clap::Parser;

use crate::IpVersion;
use crate::header::IndexPolicy;

#[derive(Parser, Debug)]
pub struct Command {
    /// ip source region txt filepath
    #[arg(long)]
    pub src: String,
    /// generated xdb filepath
    #[clap(long)]
    pub dst: String,
    #[clap(long, value_enum)]
    pub ip_version: IpVersion,
    /// index cache policy
    #[clap(long, value_enum, default_value_t = IndexPolicy::VectorIndex)]
    pub index_policy: IndexPolicy,
    /// region filter fields, the index of the fields, e.g. `1,2,3,5`
    #[clap(long, value_delimiter = ',')]
    pub filter_fields: Vec<usize>,
}
