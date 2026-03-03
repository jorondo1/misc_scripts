"""
Robust Pickle to JSON Converter with multiple fallback methods
Usage: python pkl2json.py <pickle_file> [options]

Options:
    --protocol <num>    Try specific pickle protocol (0-5)
    --encoding <enc>    Try specific encoding (default: utf-8)
    --partial           Attempt partial recovery of truncated files
    --fix-errors        Try to handle non-serializable objects
"""

import pickle
import json
import sys
import os
import argparse
from pathlib import Path


def try_standard_load(filepath, encoding='utf-8'):
    """Standard pickle load approach"""
    try:
        with open(filepath, 'rb') as f:
            obj = pickle.load(f, encoding=encoding)
        print("✓ Standard load successful")
        return obj
    except Exception as e:
        print(f"✗ Standard load failed: {e}")
        return None


def try_protocol_load(filepath, protocol=None):
    """Try loading with specific pickle protocol"""
    try:
        with open(filepath, 'rb') as f:
            if protocol is not None:
                pickle.HIGHEST_PROTOCOL = protocol
            obj = pickle.load(f)
        print(f"✓ Protocol {protocol} load successful")
        return obj
    except Exception as e:
        print(f"✗ Protocol load failed: {e}")
        return None


def try_partial_recovery(filepath):
    """Attempt to recover data from truncated pickle file"""
    print("\nAttempting partial recovery...")
    try:
        with open(filepath, 'rb') as f:
            data = f.read()
        
        total_size = len(data)
        print(f"File size: {total_size} bytes")
        
        # Try from full size down in decreasing chunks
        step_size = max(1000, total_size // 100)
        
        for size in range(total_size, 0, -step_size):
            try:
                obj = pickle.loads(data[:size])
                print(f"✓ Recovered {size} bytes ({size/total_size*100:.1f}% of file)")
                return obj
            except:
                continue
        
        print("✗ Partial recovery failed")
        return None
    except Exception as e:
        print(f"✗ Recovery error: {e}")
        return None


def convert_to_serializable(obj):
    """Convert non-JSON-serializable objects to strings"""
    if hasattr(obj, '__dict__'):
        return str(obj)
    elif hasattr(obj, 'tolist'):  # numpy arrays
        return obj.tolist()
    elif isinstance(obj, bytes):
        try:
            return obj.decode('utf-8')
        except:
            return obj.hex()
    else:
        return str(obj)


def save_json(obj, output_path, fix_errors=False):
    """Save object as JSON with error handling"""
    try:
        # Try standard JSON dump first
        json_str = json.dumps(obj, indent=2, ensure_ascii=False)
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(json_str)
        print(f"✓ JSON saved to: {output_path}")
        return True
    except (TypeError, ValueError) as e:
        if fix_errors:
            print(f"⚠ Standard JSON conversion failed, trying with error fixes...")
            try:
                json_str = json.dumps(obj, indent=2, ensure_ascii=False, 
                                     default=convert_to_serializable)
                with open(output_path, 'w', encoding='utf-8') as f:
                    f.write(json_str)
                print(f"✓ JSON saved with fixes to: {output_path}")
                return True
            except Exception as e2:
                print(f"✗ JSON conversion failed: {e2}")
                return False
        else:
            print(f"✗ JSON conversion failed: {e}")
            print("  Try using --fix-errors option")
            return False


def main():
    parser = argparse.ArgumentParser(description='Convert pickle files to JSON')
    parser.add_argument('pickle_file', help='Path to pickle file')
    parser.add_argument('--protocol', type=int, help='Pickle protocol to try')
    parser.add_argument('--encoding', default='utf-8', help='Encoding to use')
    parser.add_argument('--partial', action='store_true', 
                       help='Attempt partial recovery')
    parser.add_argument('--fix-errors', action='store_true',
                       help='Handle non-serializable objects')
    parser.add_argument('-o', '--output', help='Output JSON file path')
    
    args = parser.parse_args()
    
    # Validate input file
    if not os.path.exists(args.pickle_file):
        print(f"✗ Error: File not found: {args.pickle_file}")
        return 1
    
    print(f"Processing: {args.pickle_file}")
    print(f"File size: {os.path.getsize(args.pickle_file) / 1024 / 1024:.2f} MB\n")
    
    # Try loading the pickle file
    obj = None
    
    # Method 1: Standard load
    obj = try_standard_load(args.pickle_file, args.encoding)
    
    # Method 2: Try specific protocol
    if obj is None and args.protocol is not None:
        obj = try_protocol_load(args.pickle_file, args.protocol)
    
    # Method 3: Partial recovery
    if obj is None and args.partial:
        obj = try_partial_recovery(args.pickle_file)
    
    # If all methods failed
    if obj is None:
        print("\n✗ All loading methods failed")
        print("\nSuggestions:")
        print("  1. Verify the file is a valid pickle file")
        print("  2. Try --partial flag for truncated files")
        print("  3. Check if file was corrupted during transfer")
        print("  4. Re-download or re-copy the original file")
        return 1
    
    # Determine output path
    if args.output:
        output_path = args.output
    else:
        output_path = Path(args.pickle_file).with_suffix('.json')
    
    # Convert to JSON
    print(f"\nConverting to JSON...")
    success = save_json(obj, output_path, args.fix_errors)
    
    return 0 if success else 1


if __name__ == '__main__':
    sys.exit(main())