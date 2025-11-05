import setuptools

setuptools.setup(
    name="py-ip2region",
    version="3.0.2",
    description="ip2region official python binding with both IPv4 and IPv6 supported",
    long_description=open("ReadMe.md", encoding='utf-8').read(),
    long_description_content_type='text/markdown',

    url="https://github.com/lionsoul2014/ip2region",
    license="Apache-2.0 License",
    author_email="chenxin619315@gmail.com",
    author="lionsoul2014",

    packages=setuptools.find_packages(),
    include_package_data=True,
    install_requires=[],
    classifiers=(
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
    ),
    keywords=(
        "ip2region",
        "ip-address",
        "ip-region",
        "ip-location",
        "ip-lookup",
        "ip-search",
        "ipv4-address",
        "ipv4-region",
        "ipv4-location",
        "ipv4-lookup",
        "ipv4-search",
        "ipv6-address",
        "ipv6-region",
        "ipv6-location",
        "ipv6-lookup",
        "ipv6-search"
    )
)
