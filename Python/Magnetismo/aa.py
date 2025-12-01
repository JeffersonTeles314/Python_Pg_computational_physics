# Input data
lines = [
    "? belly button 105170",
    "? below view 4670",
    "? blonde eyebrows 4665",
    "? blonde female 100520",
    "? blonde hair 1255384",
    "? blue lipstick 17980",
    "? bob cut 52873"
]

# Extract only the tag text
tags_only = []
for line in lines:
    # Remove leading "?" and split the rest
    parts = line.lstrip("?").rsplit(" ", 1)
    if len(parts) == 2:
        tag, _ = parts
        tags_only.append(tag.strip())

# Output
for tag in tags_only:
    print(tag)
