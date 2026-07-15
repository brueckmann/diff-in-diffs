#!/bin/bash

# Shell script to run all required analysis scripts consecutively
# This script executes the analysis pipeline in the correct order

set -e  # Exit on any error

# Define script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "=========================================="
echo "Starting Analysis Pipeline"
echo "=========================================="
echo "Working directory: $SCRIPT_DIR"
echo ""

# Run data cleaning
echo "Step 1: Running data cleaning..."
echo "Executing: 01_data_cleaning.R"
Rscript "$SCRIPT_DIR/01_data_cleaning.R"
if [ $? -eq 0 ]; then
    echo "✓ Data cleaning completed successfully"
else
    echo "✗ Data cleaning failed"
    exit 1
fi
echo ""

# Run descriptive statistics
echo "Step 2: Running descriptive statistics..."
echo "Executing: 02_descriptives.R"
Rscript "$SCRIPT_DIR/02_descriptives.R"
if [ $? -eq 0 ]; then
    echo "✓ Descriptive statistics completed successfully"
else
    echo "✗ Descriptive statistics failed"
    exit 1
fi
echo ""

# Run modeling
echo "Step 3: Running modeling analysis..."
echo "Executing: 03_modeling.R"
Rscript "$SCRIPT_DIR/03_modeling.R"
if [ $? -eq 0 ]; then
    echo "✓ Modeling analysis completed successfully"
else
    echo "✗ Modeling analysis failed"
    exit 1
fi
echo ""

# Run final code processing
echo "Step 4: Running final code processing..."
echo "Executing: 99_code.R"
Rscript "$SCRIPT_DIR/99_code.R"
if [ $? -eq 0 ]; then
    echo "✓ Final code processing completed successfully"
else
    echo "✗ Final code processing failed"
    exit 1
fi
echo ""

echo "=========================================="
echo "Analysis Pipeline Completed Successfully!"
echo "=========================================="

