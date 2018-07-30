let node_ver = process.version
// Because nodejs's version is something like `v8.11.3`. So we should ignore `v` first
node_ver = node_ver.substr(1);
// Splitted by `.`
node_ver = node_ver.split('.');
// Take the main version number
node_ver = parseInt(node_ver[0]);

module.exports = node_ver;
