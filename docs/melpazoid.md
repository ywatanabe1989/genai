<!-- ---
!-- title: 2024-12-23 14:12:38
!-- author: ywata-note-win
!-- date: /home/ywatanabe/.dotfiles/.emacs.d/lisp/genai/docs/melpazoid.md
!-- --- -->

# MELPA Package Development Guide

## Prerequisites
- Docker installed and running
- Git installed
- Access to Unix/Linux environment
- Emacs installed

## Package Validation Steps

### 1. Byte Compilation Test
```bash
function verify_byte_compilation() {
    local package_path=$1
    
    if [ ! -f "$package_path" ]; then
        echo "Error: Package file not found at $package_path"
        return 1
    fi

    emacs -Q --batch \
    --eval '(require '\''package)' \
    --eval '(add-to-list '\''package-archives '\''("melpa" . "https://melpa.org/packages/"))' \
    --eval '(package-initialize)' \
    --eval '(package-refresh-contents)' \
    -f batch-byte-compile "$package_path"

    local elc_file="${package_path}c"
    if [ -f "$elc_file" ]; then
        echo -e "${YELLOW}Success: Byte compilation completed${NC}"
        return 0
    else
        echo -e "${RED}Error: Byte compilation failed${NC}"
        return 1
    fi
}

PACKAGE_PATH="/home/ywatanabe/.emacs.d/lisp/genai/genai.el"
verify_byte_compilation "$PACKAGE_PATH"
```

### 2. Melpazoid Check

``` bash
function check_melpazoid() {
    local repo_owner=$1
    local repo_name=$2
    local pr_number=$3
    local temp_melpa_dir="/tmp/melpazoid"

    # First time setup - run these before check_melpazoid
    docker build -t melpazoid https://github.com/riscy/melpazoid.git
    docker run -d --name melpazoid melpazoid tail -f /dev/null

    # Clone and setup melpazoid
    cd /tmp
    if [ ! -d "$temp_melpa_dir" ]; then
        git clone https://github.com/riscy/melpazoid.git
        cd melpazoid
        sudo mkdir -p "$temp_melpa_dir"
        sudo chown -R emacser:emacser "$temp_melpa_dir/_requirements.el"
        sudo chown -R emacser:emacser "$temp_melpa_dir"
        sudo chmod -R 777 "$temp_melpa_dir"
    fi
    cd $temp_melpa_dir

    # Run Docker check
    local container_id=$(docker ps -aqf "name=melpazoid")
    if [ -z "$container_id" ]; then
        echo "Error: Docker container not found"
        return 1
    fi

    docker start "$container_id"
    
    # Prepare environment variables
    local melpa_pr="https://github.com/melpa/melpa/pull/$pr_number"
    local recipe="($repo_name :repo \"$repo_owner/$repo_name\" :fetcher github)"
    
    # Execute check
    docker exec -it "$container_id" /bin/bash -c "
        export MELPA_PR_URL=$melpa_pr
        RECIPE='$recipe' make
    "

    # Check results
    if [ -f "$temp_melpa_dir/pkg/$repo_name.el" ]; then
        echo -e "${YELLOW}Success: Melpazoid check completed{NC}"
        return 0
    else
        echo -e "${RED}Error: Melpazoid check failed${NC}"
        return 1
    fi
}

# Then run your check
RECIPE='(genai :repo "ywatanabe1989/genai" :fetcher github)'
check_melpazoid "ywatanabe1989" "genai" "9084" "$RECIPE"
```

## Combined

``` bash
# Define colors for output
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

function validate_melpa_package() {
    local package_name=$1
    local package_path=$2
    local repo_owner=$3
    local repo_name=$4
    local pr_number=$5
    local recipe="($package_name :repo \"$repo_owner/$repo_name\" :fetcher github)"

    echo "Running byte compilation test..."
    if ! verify_byte_compilation "$package_path"; then
        echo -e "${RED}Byte compilation failed${NC}"
        return 1
    fi

    echo "Running melpazoid check..."
    if ! check_melpazoid "$repo_owner" "$repo_name" "$pr_number" "$recipe"; then
        echo -e "${RED}Melpazoid check failed${NC}"
        return 1
    fi

    echo -e "${YELLOW}All validation steps passed successfully${NC}"
    return 0
}

# Usage
PACKAGE_NAME="genai"
PACKAGE_PATH="/home/ywatanabe/.emacs.d/lisp/genai/genai.el"
GITHUB_USER="ywatanabe1989"
GITHUB_REPO="genai"
MELPA_PR_NUMBER="9084"

validate_melpa_package \
    $PACKAGE_NAME \
    $PACKAGE_PATH \
    $GITHUB_USER \
    $GITHUB_REPO \
    $MELPA_PR_NUMBER
```