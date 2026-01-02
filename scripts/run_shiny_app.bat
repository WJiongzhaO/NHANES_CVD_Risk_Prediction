@echo off
echo ========================================
echo 启动NHANES健康风险预测Shiny应用
echo ========================================
echo.

cd /d "%~dp0\.."

echo 正在启动Shiny应用...
echo 注意: 应用将在浏览器中自动打开
echo 按 Ctrl+C 可以停止应用
echo.

REM 尝试多个可能的R路径
set RSCRIPT_PATH=
if exist "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" (
    set RSCRIPT_PATH=C:\Program Files\R\R-4.5.1\bin\Rscript.exe
) else if exist "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" (
    set RSCRIPT_PATH=C:\Program Files\R\R-4.4.1\bin\Rscript.exe
) else if exist "C:\Program Files\R\R-4.3.3\bin\Rscript.exe" (
    set RSCRIPT_PATH=C:\Program Files\R\R-4.3.3\bin\Rscript.exe
) else (
    REM 尝试使用系统PATH中的Rscript
    where Rscript.exe >nul 2>&1
    if %errorlevel% equ 0 (
        set RSCRIPT_PATH=Rscript.exe
    ) else (
        echo 错误: 找不到Rscript.exe，请确保R已安装并在PATH中
        pause
        exit /b 1
    )
)

echo 使用R路径: %RSCRIPT_PATH%
echo.

REM 检查必要的包
echo 检查Shiny相关包...
"%RSCRIPT_PATH%" -e "if (!require('shiny', quietly = TRUE)) { install.packages('shiny', dependencies = TRUE) }"
"%RSCRIPT_PATH%" -e "if (!require('shinydashboard', quietly = TRUE)) { install.packages('shinydashboard', dependencies = TRUE) }"
"%RSCRIPT_PATH%" -e "if (!require('plotly', quietly = TRUE)) { install.packages('plotly', dependencies = TRUE) }"
"%RSCRIPT_PATH%" -e "if (!require('DT', quietly = TRUE)) { install.packages('DT', dependencies = TRUE) }"

echo.
echo ========================================
echo 启动Shiny应用
echo ========================================
echo.
echo 应用将在浏览器中自动打开
echo 默认地址: http://127.0.0.1:3838
echo.
echo 按 Ctrl+C 可以停止应用
echo ========================================
echo.

REM 运行Shiny应用
"%RSCRIPT_PATH%" scripts\06_shiny_app.R

if %errorlevel% neq 0 (
    echo.
    echo ========================================
    echo 错误: Shiny应用启动失败！
    echo ========================================
    echo.
    echo 请检查:
    echo   1. 模型文件是否存在（请先运行建模脚本）
    echo   2. 数据文件是否存在（请先运行数据清洗脚本）
    echo   3. 所有必要的R包是否已安装
    echo.
    pause
    exit /b 1
)

pause

