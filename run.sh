#!/bin/bash

# Shell script to run all required analysis scripts consecutively
# This script executes the analysis pipeline in the correct order

set -e  # Exit on any error

# Define script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/02_rscripts"

echo "=========================================="
echo "Starting Analysis Pipeline"
echo "=========================================="
echo "Script working directory: $SCRIPT_DIR"
echo ""

# Install R packages
echo "Installing R packages..."
Rscript -e "renv::restore()"
echo "✓ Packages installed"
echo ""

# Get data from Harvard dataverse 
echo "Step 0: Running get data..."
echo "Executing: 00_get_data.R"
Rscript "$SCRIPT_DIR/00_get_data.R"
if [ $? -eq 0 ]; then
    echo "✓ Data download successfully"
else
    echo "✗ Data download failed"
    exit 1
fi
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

# Run session info generating code
echo "Step 4: Running session info generating code..."
echo "Executing: 98_sessioninfo_and_cite.R"
Rscript "$SCRIPT_DIR/98_sessioninfo_and_cite.R"
if [ $? -eq 0 ]; then
    echo "✓ Make session info completed successfully"
else
    echo "✗ Make session info failed"
    exit 1
fi
echo ""

echo "=========================================="
echo "Analysis Pipeline Completed Successfully!"
echo "=========================================="
