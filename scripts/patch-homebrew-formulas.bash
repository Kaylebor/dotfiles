#!/bin/bash
# Generic Homebrew formula patching system for alternative installations

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCHES_DIR="$SCRIPT_DIR/patches"

echo "Applying Homebrew formula patches for alternative installation..."

# Check if patches directory exists
if [[ ! -d "$PATCHES_DIR" ]]; then
    echo "No patches directory found at $PATCHES_DIR, skipping"
    exit 0
fi

# Function to patch a formula
patch_formula() {
    local formula_name="$1"
    local patch_file="$2"
    
    echo "Patching formula: $formula_name"
    
    # Skip if package is already installed
    if brew list "$formula_name" &>/dev/null; then
        echo "✅ $formula_name already installed, skipping patch"
        return 0
    fi
    
    # Get formula path
    local formula_path
    formula_path="$(brew --repository homebrew/core)/Formula/${formula_name:0:1}/${formula_name}.rb"
    
    if [[ ! -f "$formula_path" ]]; then
        echo "⚠️  Formula not found: $formula_path"
        return 1
    fi
    
    # Check if already patched by looking for our specific patch content
    if grep -q "this->end()" "$formula_path"; then
        echo "✅ $formula_name already patched"
        return 0
    fi
    
    # Create backup and apply patch
    local tap_path
    tap_path="$(brew --repository homebrew/core)"
    
    echo "Backing up original formula..."
    cp "$formula_path" "$formula_path.original"
    
    echo "Applying patch from $patch_file..."
    
    # Apply the patch by modifying the formula structure
    # First, restore from git to get clean version
    git -C "$(brew --repository homebrew/core)" checkout HEAD -- "Formula/${formula_name:0:1}/${formula_name}.rb" 2>/dev/null || true
    
    # Check if formula already uses patch :DATA (aspell case)
    if grep -q "patch :DATA" "$formula_path"; then
        echo "Formula already uses patch :DATA, appending our patch..."
        # Simply append our patch data after __END__
        cat "$patch_file" >> "$formula_path"
    else
        # Formula doesn't have patches, add patches method before install
        awk -v patch_file="$patch_file" '
        /^  def install/ && !patches_added {
            print "  # PATCHED BY CHEZMOI - C++ compatibility fixes"
            print "  def patches"
            print "    DATA"
            print "  end"
            print ""
            patches_added = 1
        }
        { print }
        END {
            if (patches_added) {
                print ""
                print "__END__"
                while ((getline line < patch_file) > 0) {
                    print line
                }
                close(patch_file)
            }
        }
        ' "$formula_path" > "$formula_path.tmp" && mv "$formula_path.tmp" "$formula_path"
    fi
    
    echo "✅ Successfully patched $formula_name"
}

# Process all patch files
for patch_file in "$PATCHES_DIR"/*.patch; do
    if [[ -f "$patch_file" ]]; then
        # Extract formula name from filename (e.g., aspell.patch -> aspell)
        formula_name=$(basename "$patch_file" .patch)
        echo "Found patch for: $formula_name"
        patch_formula "$formula_name" "$patch_file"
    fi
done

echo "✅ Formula patching complete"