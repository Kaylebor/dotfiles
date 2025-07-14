# Future Enhancements for Homebrew Formula Patching

## Hash Verification System

### Problem
We need to verify that we're patching the expected version of a formula file to avoid conflicts when upstream formulas are updated.

### Proposed Solution
Add configuration files alongside patches that include:

1. **Expected SHA256 hash** of the original formula file
2. **Description** of what the patch does
3. **Version compatibility** information

### Implementation Plan

1. Create `.config` files alongside `.patch` files:
   ```
   scripts/patches/
   ├── aspell.patch
   ├── aspell.config
   ├── someother.patch
   └── someother.config
   ```

2. Config file format:
   ```bash
   # Expected SHA256 hash of the original formula file
   EXPECTED_HASH="a1b2c3d4e5f6..."
   # Description of what this patch does
   DESCRIPTION="Fix C++ compatibility issues..."
   # Compatible formula versions (optional)
   COMPATIBLE_VERSIONS="0.60.8.1"
   ```

3. Update `patch-homebrew-formulas.bash` to:
   - Calculate hash of original formula before patching
   - Compare with expected hash from config
   - Fail with clear error if hashes don't match
   - Suggest updating the patch for new formula version

### Benefits
- **Safety**: Prevents patching wrong formula versions
- **Maintenance**: Clear indication when patches need updating
- **Documentation**: Self-documenting patch purpose and compatibility
- **CI/CD Ready**: Can be integrated into automated checks

### Error Handling
When hash mismatch occurs:
1. Show clear error message with expected vs actual hash
2. Suggest checking if formula was updated upstream
3. Provide instructions for updating the patch
4. Option to force patch anyway (with warning)

---

*Created: 2025-07-14*
*Status: Future enhancement - not yet implemented*