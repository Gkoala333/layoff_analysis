-- 第1部分：数据清洗和准备
-- ========================================

-- 步骤1：创建临时表保存原始数据
DROP TABLE IF EXISTS layoffs_staging;
CREATE TABLE layoffs_staging AS
SELECT * FROM layoffs_data;

-- 步骤2：清洗和标准化数据，删除重复项
DROP TABLE IF EXISTS layoffs_clean;

CREATE TABLE layoffs_clean AS
WITH cleaned_data AS (
    SELECT 
        -- 清理所有文本字段的空格
        TRIM(Company) AS Company,
        TRIM(Location_HQ) AS Location_HQ,
        
        -- 处理缺失或空白的行业字段
        CASE 
            WHEN TRIM(COALESCE(Industry, '')) = '' THEN 'Unknown'
            ELSE TRIM(Industry)
        END AS Industry,
        
        -- 清理数值字段
        CASE 
            WHEN Laid_Off_Count IS NULL OR Laid_Off_Count = 0 THEN NULL
            ELSE Laid_Off_Count
        END AS Laid_Off_Count,
        
        -- 转换日期字符串为DATE格式 (SQLite使用文本存储日期)
        -- 假设格式为 M/D/YYYY 或 MM/DD/YYYY
        CASE 
            WHEN Date LIKE '%/%/%' THEN 
                -- 转换为 YYYY-MM-DD 格式
                substr(Date, -4) || '-' || 
                printf('%02d', CAST(substr(Date, 1, instr(Date, '/') - 1) AS INTEGER)) || '-' ||
                printf('%02d', CAST(substr(substr(Date, instr(Date, '/') + 1), 1, 
                    instr(substr(Date, instr(Date, '/') + 1), '/') - 1) AS INTEGER))
            WHEN Date IS NOT NULL AND Date != '' THEN Date
            ELSE NULL
        END AS Layoff_Date,
        
        TRIM(Source) AS Source,
        
        -- 清理募资金额（移除非数值，处理空值）
        CASE 
            WHEN Funds_Raised IS NULL OR Funds_Raised = '' THEN NULL
            ELSE CAST(Funds_Raised AS REAL)
        END AS Funds_Raised,
        
        -- 标准化阶段名称
        CASE 
            WHEN TRIM(COALESCE(Stage, '')) = '' THEN 'Unknown'
            ELSE TRIM(Stage)
        END AS Stage,
        
        -- 转换添加日期为正确格式
        CASE 
            WHEN Date_Added LIKE '%/%/%' THEN 
                substr(Date_Added, -4) || '-' || 
                printf('%02d', CAST(substr(Date_Added, 1, instr(Date_Added, '/') - 1) AS INTEGER)) || '-' ||
                printf('%02d', CAST(substr(substr(Date_Added, instr(Date_Added, '/') + 1), 1, 
                    instr(substr(Date_Added, instr(Date_Added, '/') + 1), '/') - 1) AS INTEGER))
            ELSE Date_Added
        END AS Date_Added,
        
        -- 标准化国家名称（修复常见变体）
        CASE TRIM(COALESCE(Country, ''))
            WHEN 'United States' THEN 'United States'
            WHEN 'US' THEN 'United States'
            WHEN 'USA' THEN 'United States'
            WHEN 'United Kingdom' THEN 'United Kingdom'
            WHEN 'UK' THEN 'United Kingdom'
            WHEN '' THEN 'Unknown'
            ELSE TRIM(Country)
        END AS Country,
        
        -- 清理百分比字段
        CASE 
            WHEN Percentage IS NULL OR Percentage = '' THEN NULL
            ELSE CAST(Percentage AS REAL)
        END AS Percentage_Laid_Off,
        
        TRIM(COALESCE(List_of_Employees_Laid_Off, '')) AS Employee_List
        
    FROM layoffs_staging
),
-- 步骤3：使用ROW_NUMBER()窗口函数删除重复项
deduplicated AS (
    SELECT *,
        ROW_NUMBER() OVER (
            PARTITION BY Company, Location_HQ, Industry, Laid_Off_Count, Layoff_Date
            ORDER BY Date_Added DESC, Source
        ) AS row_num
    FROM cleaned_data
)
-- 最终清洗后的数据集，已删除重复项
SELECT 
    Company,
    Location_HQ,
    Industry,
    Laid_Off_Count,
    Layoff_Date,
    Source,
    Funds_Raised,
    Stage,
    Date_Added,
    Country,
    Percentage_Laid_Off,
    Employee_List
FROM deduplicated
WHERE row_num = 1  -- 只保留重复项的第一次出现
    AND Laid_Off_Count IS NOT NULL  -- 过滤掉没有裁员人数的记录
    AND Layoff_Date IS NOT NULL;    -- 过滤掉没有日期的记录

-- 创建索引以提高查询性能
CREATE INDEX IF NOT EXISTS idx_layoff_date ON layoffs_clean(Layoff_Date);
CREATE INDEX IF NOT EXISTS idx_industry ON layoffs_clean(Industry);
CREATE INDEX IF NOT EXISTS idx_country ON layoffs_clean(Country);
CREATE INDEX IF NOT EXISTS idx_company ON layoffs_clean(Company);

-- ========================================
-- 第2部分：汇总分析
-- ========================================

DROP TABLE IF EXISTS layoffs_summary;
CREATE TABLE layoffs_summary AS
SELECT 
    Industry,
    Country,
    -- 汇总指标
    COUNT(*) AS Total_Events,
    SUM(Laid_Off_Count) AS Total_Layoffs,
    ROUND(AVG(Laid_Off_Count), 2) AS Avg_Layoffs_Per_Event,
    MAX(Laid_Off_Count) AS Max_Single_Layoff,
    MIN(Laid_Off_Count) AS Min_Single_Layoff,
    
    -- 财务指标
    COUNT(Funds_Raised) AS Companies_With_Funding_Data,
    ROUND(AVG(Funds_Raised), 2) AS Avg_Funds_Raised,
    
    -- 日期范围
    MIN(Layoff_Date) AS First_Layoff_Date,
    MAX(Layoff_Date) AS Last_Layoff_Date
     
FROM layoffs_clean
GROUP BY Industry, Country
HAVING SUM(Laid_Off_Count) > 0  -- 只包括有实际裁员的组合
ORDER BY Total_Layoffs DESC;

-- 创建按行业汇总的表
DROP TABLE IF EXISTS layoffs_by_industry;
CREATE TABLE layoffs_by_industry AS
SELECT 
    Industry,
    COUNT(*) AS Total_Events,
    SUM(Laid_Off_Count) AS Total_Layoffs,
    ROUND(AVG(Laid_Off_Count), 2) AS Avg_Layoffs,
    COUNT(DISTINCT Country) AS Countries_Affected,
    COUNT(DISTINCT Company) AS Companies_Affected
FROM layoffs_clean
GROUP BY Industry
ORDER BY Total_Layoffs DESC;

-- 创建按国家汇总的表
DROP TABLE IF EXISTS layoffs_by_country;
CREATE TABLE layoffs_by_country AS
SELECT 
    Country,
    COUNT(*) AS Total_Events,
    SUM(Laid_Off_Count) AS Total_Layoffs,
    ROUND(AVG(Laid_Off_Count), 2) AS Avg_Layoffs,
    COUNT(DISTINCT Industry) AS Industries_Affected,
    COUNT(DISTINCT Company) AS Companies_Affected
FROM layoffs_clean
GROUP BY Country
ORDER BY Total_Layoffs DESC;

-- ========================================
-- 第3部分：滚动平均分析
-- ========================================

DROP TABLE IF EXISTS layoffs_rolling_avg;
CREATE TABLE layoffs_rolling_avg AS
WITH monthly_aggregates AS (
    -- 首先按行业和月份汇总裁员数据
    SELECT 
        Industry,
        -- SQLite使用strftime函数格式化日期
        date(Layoff_Date, 'start of month') AS Month_Start,
        SUM(Laid_Off_Count) AS Monthly_Layoffs,
        COUNT(*) AS Event_Count,
        COUNT(DISTINCT Company) AS Companies_Affected
    FROM layoffs_clean
    WHERE Layoff_Date IS NOT NULL
    GROUP BY Industry, date(Layoff_Date, 'start of month')
),
rolling_calculations AS (
    -- 使用窗口函数计算3个月滚动平均值
    SELECT 
        Industry,
        Month_Start,
        Monthly_Layoffs,
        Event_Count,
        Companies_Affected,
        
        -- 3个月滚动平均（当前月+前2个月）
        ROUND(AVG(Monthly_Layoffs) OVER (
            PARTITION BY Industry 
            ORDER BY Month_Start 
            ROWS BETWEEN 2 PRECEDING AND CURRENT ROW
        ), 2) AS Rolling_3Month_Avg,
        
        -- 使用LAG获取上个月裁员数
        LAG(Monthly_Layoffs, 1) OVER (
            PARTITION BY Industry 
            ORDER BY Month_Start
        ) AS Previous_Month_Layoffs,
        
        -- 使用LEAD获取下个月裁员数
        LEAD(Monthly_Layoffs, 1) OVER (
            PARTITION BY Industry 
            ORDER BY Month_Start
        ) AS Next_Month_Layoffs,
        
        -- 月度环比变化
        Monthly_Layoffs - LAG(Monthly_Layoffs, 1) OVER (
            PARTITION BY Industry 
            ORDER BY Month_Start
        ) AS MoM_Change,
        
        -- 按行业累计裁员数
        SUM(Monthly_Layoffs) OVER (
            PARTITION BY Industry 
            ORDER BY Month_Start
        ) AS Cumulative_Layoffs,
        
        -- 移动总和（3个月总计）
        SUM(Monthly_Layoffs) OVER (
            PARTITION BY Industry 
            ORDER BY Month_Start 
            ROWS BETWEEN 2 PRECEDING AND CURRENT ROW
        ) AS Rolling_3Month_Total
        
    FROM monthly_aggregates
)
SELECT 
    Industry,
    Month_Start,
    Monthly_Layoffs,
    Event_Count,
    Companies_Affected,
    Rolling_3Month_Avg,
    Previous_Month_Layoffs,
    Next_Month_Layoffs,
    MoM_Change,
    
    -- 计算月度环比百分比变化
    CASE 
        WHEN Previous_Month_Layoffs > 0 THEN
            ROUND(((Monthly_Layoffs - Previous_Month_Layoffs) * 100.0 / Previous_Month_Layoffs), 2)
        ELSE NULL
    END AS MoM_Pct_Change,
    
    Cumulative_Layoffs,
    Rolling_3Month_Total
FROM rolling_calculations
ORDER BY Industry, Month_Start;

-- ========================================
-- 第4部分：最近6个月前5大行业
-- ========================================

DROP TABLE IF EXISTS top_industries_6months;
CREATE TABLE top_industries_6months AS
WITH max_date AS (
    SELECT MAX(Layoff_Date) AS latest_date
    FROM layoffs_clean
),
last_6_months AS (
    -- 计算最近6个月的数据（从最近一次裁员往前推6个月）
    SELECT 
        Industry,
        SUM(Laid_Off_Count) AS Total_Layoffs_6M,
        COUNT(*) AS Total_Events_6M,
        COUNT(DISTINCT Company) AS Companies_Affected_6M,
        ROUND(AVG(Laid_Off_Count), 2) AS Avg_Layoffs_Per_Event_6M,
        MIN(Layoff_Date) AS First_Layoff_6M,
        MAX(Layoff_Date) AS Last_Layoff_6M
    FROM layoffs_clean
    WHERE Layoff_Date >= (
        SELECT date(latest_date, '-6 months')
        FROM max_date
    )
    GROUP BY Industry
),
total_sum AS (
    SELECT SUM(Total_Layoffs_6M) AS grand_total
    FROM last_6_months
),
ranked_industries AS (
    SELECT 
        l.*,
        ROW_NUMBER() OVER (ORDER BY Total_Layoffs_6M DESC) AS Industry_Rank
    FROM last_6_months l
)
SELECT 
    Industry_Rank,
    Industry,
    Total_Layoffs_6M,
    Total_Events_6M,
    Companies_Affected_6M,
    Avg_Layoffs_Per_Event_6M,
    First_Layoff_6M,
    Last_Layoff_6M,
    
    -- 计算占总裁员的百分比
    ROUND((Total_Layoffs_6M * 100.0 / (SELECT grand_total FROM total_sum)), 2) 
        AS Pct_Of_Total_Layoffs
        
FROM ranked_industries
WHERE Industry_Rank <= 5
ORDER BY Industry_Rank;

-- ========================================
-- 第5部分：数据质量和汇总视图
-- ========================================

-- 视图：数据质量指标
DROP VIEW IF EXISTS data_quality_report;
CREATE VIEW data_quality_report AS
SELECT 
    'Total Records in Original Data' AS Metric,
    CAST(COUNT(*) AS TEXT) AS Value
FROM layoffs_data
UNION ALL
SELECT 
    'Records After Cleaning',
    CAST(COUNT(*) AS TEXT)
FROM layoffs_clean
UNION ALL
SELECT 
    'Duplicate Records Removed',
    CAST((SELECT COUNT(*) FROM layoffs_data) - (SELECT COUNT(*) FROM layoffs_clean) AS TEXT)
UNION ALL
SELECT 
    'Records with Missing Industry',
    CAST(COUNT(*) AS TEXT)
FROM layoffs_clean
WHERE Industry = 'Unknown'
UNION ALL
SELECT 
    'Records with Missing Country',
    CAST(COUNT(*) AS TEXT)
FROM layoffs_clean
WHERE Country = 'Unknown'
UNION ALL
SELECT 
    'Date Range (Days)',
    CAST(julianday(MAX(Layoff_Date)) - julianday(MIN(Layoff_Date)) AS TEXT)
FROM layoffs_clean;

-- 视图：总体汇总统计
DROP VIEW IF EXISTS overall_summary;
CREATE VIEW overall_summary AS
SELECT 
    COUNT(DISTINCT Company) AS Total_Companies,
    COUNT(DISTINCT Industry) AS Total_Industries,
    COUNT(DISTINCT Country) AS Total_Countries,
    SUM(Laid_Off_Count) AS Total_Layoffs,
    ROUND(AVG(Laid_Off_Count), 2) AS Avg_Layoffs_Per_Event,
    MAX(Laid_Off_Count) AS Largest_Single_Layoff,
    (SELECT Company FROM layoffs_clean 
     ORDER BY Laid_Off_Count DESC LIMIT 1) AS Company_Largest_Layoff,
    MIN(Layoff_Date) AS Earliest_Date,
    MAX(Layoff_Date) AS Latest_Date
FROM layoffs_clean;

-- ========================================
-- 验证查询
-- ========================================

-- 显示清洗后数据样本
SELECT '=== Sample Cleaned Data ===' AS Report;
SELECT * FROM layoffs_clean;

-- 显示汇总数据
SELECT '=== Top 10 Industries by Total Layoffs ===' AS Report;
SELECT * FROM layoffs_by_industry;

-- 显示行业的滚动平均值
SELECT '=== Rolling Average ===' AS Report;
SELECT * FROM layoffs_rolling_avg 
WHERE Industry = (SELECT Industry FROM layoffs_by_industry LIMIT 1)
ORDER BY Month_Start DESC;

-- 显示最近6个月前5大行业
SELECT '=== Top 5 Industries (Last 6 Months) ===' AS Report;
SELECT * FROM top_industries_6months;

-- 显示数据质量报告
SELECT '=== Data Quality Report ===' AS Report;
SELECT * FROM data_quality_report;

-- 显示总体汇总
SELECT '=== Overall Summary ===' AS Report;
SELECT * FROM overall_summary;