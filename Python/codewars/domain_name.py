import re
def domain_name(url):
    if "www." in url:
        url = url.replace("www.", "")
    return re.search("\w{4,5}:\/\/([\w-]+)\.[a-z]{2,4}", url).group(1) if "http" in url else re.search("([\w-]+)\.[a-z]{2,4}", url).group(1)
