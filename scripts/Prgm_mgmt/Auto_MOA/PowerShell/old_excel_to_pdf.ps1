param(
    [string]$inputFile,
    [string]$outputFile
)

# Start Excel
$excel = New-Object -ComObject Excel.Application
$excel.Visible = $false
$excel.DisplayAlerts = $false

try {
    # Open workbook
    $workbook = $excel.Workbooks.Open($inputFile)

    # Select sheet (2 = Requested Funds Budget)
    $worksheet = $workbook.Worksheets.Item(2)

    # ---- FIX PRINT ISSUES ----
    $worksheet.ResetAllPageBreaks()

    # Define printable area properly
    $usedRange = $worksheet.UsedRange
    $worksheet.PageSetup.PrintArea = $usedRange.Address()

    # ---- PAGE LAYOUT SETTINGS ----
    $worksheet.PageSetup.Orientation = 2  # 2 = Portrait

    # Force EVERYTHING onto ONE page
    $worksheet.PageSetup.Zoom = $false
    $worksheet.PageSetup.FitToPagesWide = 1
    $worksheet.PageSetup.FitToPagesTall = 1

    # Optional margins (slightly tighter = better fit)
    $worksheet.PageSetup.LeftMargin = 36
    $worksheet.PageSetup.RightMargin = 36
    $worksheet.PageSetup.TopMargin = 36
    $worksheet.PageSetup.BottomMargin = 36

    # ---- EXPORT ----
    $worksheet.ExportAsFixedFormat(0, $outputFile)

}
finally {
    # Close workbook without saving
    if ($workbook) {
        $workbook.Close($false)
    }

    # Quit Excel
    $excel.Quit()

    # Cleanup COM objects
    if ($worksheet) {
        [System.Runtime.Interopservices.Marshal]::ReleaseComObject($worksheet) | Out-Null
    }
    if ($workbook) {
        [System.Runtime.Interopservices.Marshal]::ReleaseComObject($workbook) | Out-Null
    }
    if ($excel) {
        [System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null
    }

    [GC]::Collect()
    [GC]::WaitForPendingFinalizers()
}