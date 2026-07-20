#!/bin/bash

# Shell script to run all required analysis scripts consecutively
# This script executes the analysis pipeline in the correct order

set -e  # Exit on any error

# Define script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/02_rscripts"

# Check if Rscript is available, if not prompt user for R installation path
if ! command -v Rscript &> /dev/null; then
    echo "=========================================="
    echo "Rscript not found in PATH"
    echo "=========================================="
    echo ""
    echo "R is installed on your system but Rscript is not in your PATH."
    echo "This commonly happens on Windows when R is installed without admin privileges."
    echo ""
    echo "To find your R installation path:"
    echo "  1. Open R GUI (Rgui.exe)"
    echo "  2. Run: R.home()"
    echo "  3. Copy the entire path shown (e.g., C:\\Program Files\\R\\R-4.6.1)"
    echo "     and paste it below (it's OK if it shows as PROGRA~1, that's normal)"
    echo ""
    read -p "Please enter your R installation path: " R_HOME_INPUT
    
    # Remove quotes if user accidentally included them
    R_HOME_INPUT="${R_HOME_INPUT%\"}"
    R_HOME_INPUT="${R_HOME_INPUT#\"}"
    
    # Convert to Unix path format using cygpath (works in Git Bash and MinGW)
    if command -v cygpath &> /dev/null; then
        R_HOME=$(cygpath -u "$R_HOME_INPUT")
    else
        # Fallback: manual conversion for other bash environments
        R_HOME="${R_HOME_INPUT//\\//}"
    fi
    
    # Set Rscript to full path
    RSCRIPT="$R_HOME/bin/Rscript.exe"
    
    # Verify the path is correct by checking if the file exists
    if [ ! -f "$RSCRIPT" ]; then
        echo ""
        echo "✗ Error: Rscript.exe not found at:"
        echo "  $RSCRIPT"
        echo ""
        echo "Please verify the path you entered. Common locations:"
        echo "  - C:\\Program Files\\R\\R-4.6.1\\bin\\Rscript.exe"
        echo "  - C:\\Program Files (x86)\\R\\R-4.6.1\\bin\\Rscript.exe"
        echo "  - C:\\Users\\[YourUsername]\\AppData\\Local\\R\\R-4.6.1\\bin\\Rscript.exe"
        exit 1
    fi
    
    echo "✓ Using R from: $R_HOME"
    echo ""
else
    RSCRIPT="Rscript"
fi

echo "=========================================="
echo "Starting Analysis Pipeline"
echo "=========================================="
echo "Script working directory: $SCRIPT_DIR"
echo ""

# Install R packages
# Install R packages
echo "Installing R packages..."
Rscript "$SCRIPT_DIR/install.R"
if [ $? -eq 0 ]; then
    echo "✓ Packages installed"
else
    echo " R Package installation failed"
    exit 1
fi
echo ""

# Get data from Harvard dataverse 
echo "Step 0: Running get data..."
echo "Executing: 00_get_data.R"
"$RSCRIPT" "$SCRIPT_DIR/00_get_data.R"
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
"$RSCRIPT" "$SCRIPT_DIR/01_data_cleaning.R"
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
"$RSCRIPT" "$SCRIPT_DIR/02_descriptives.R"
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
"$RSCRIPT" "$SCRIPT_DIR/03_modeling.R"
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
"$RSCRIPT" "$SCRIPT_DIR/98_sessioninfo_and_cite.R"
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
