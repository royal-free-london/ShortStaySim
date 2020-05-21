declare @start datetime = '20190401'
declare @end datetime   = '20200301'

--do not change @pothead_cutoff
declare @pothead_cutoff datetime = case when @start < '20180409' then '20180409' else @start end

SELECT

Attendance_Number																AS 'Attendance Number'
,Local_Patient_ID																AS 'Local_Patient_ID'
,NHS_Number                                                                     AS 'NHS Number'
,Patient_Forename+' '+Patient_Surname											AS 'Patient Name'
,Patient_Group                                                                  AS 'Patient Group'
,[Organisation_Code_(Code_of_Commissioner)]										AS 'Purchaser Code'
,Date_of_Birth                                                                  AS 'Date of Birth'
,Arrival_Date																	AS 'Arrival Date'
,Arrival_Date+' '+ae.Arrival_Time												AS 'Arrival DateTime'
,Attendance_Disposal_desc														AS 'AttendanceDisposalCode'

,case when case when Initial_Assessment_Time =0 then null else Initial_Assessment_Time end <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end+case when Initial_Assessment_Time =0 then null else Initial_Assessment_Time end as 'Initial Assessment DateTime'
,case when case when Time_Seen_for_Treatment = 0 then null else Time_Seen_for_Treatment end <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end+case when Time_Seen_for_Treatment = 0 then null else Time_Seen_for_Treatment end AS 'Time Seen for Treatment DateTime'
,case when case when ae.Attendance_Conclusion_Time =0 then null else ae.Attendance_Conclusion_Time end <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end+case when ae.Attendance_Conclusion_Time =0 then null else ae.Attendance_Conclusion_Time end AS 'Attendance Conclusion DateTime'
,case when case when Departure_Time =0 then null else Departure_Time end <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end+case when Departure_Time =0 then null else Departure_Time end AS 'Departure DateTime'
,case when ae.Arrival_Date < '20190323' then p.[ED Streaming] else [EdDischargeInformationSectionEdStreaming] end																AS 'Stream (Discharge)'
,Attendance_Category															AS 'AttendanceCategoryCode'
,Source_of_Referral																AS 'SourceOfReferralAECode'
,Arrival_Mode_Desc																AS 'ArrivalMode'

, CASE
       WHEN aeb.[Last Building Desc] IS not null THEN 
       
       CASE WHEN aeb.[Last Building Desc] = 'RFH' THEN 'Royal Free'
       WHEN aeb.[Last Building Desc] = 'BH' THEN 'Barnet'
       WHEN aeb.[Last Building Desc] = 'CFH' THEN 'Chase Farm'
       ELSE 'Other' END
       
       WHEN ae.treatment_site_code = 'ral01' THEN 'Royal Free'
       WHEN ae.treatment_site_code in ('rvl01','ral26') THEN 'Barnet'
       WHEN ae.treatment_site_code in ('rvlc7','ralc7') THEN 'Chase Farm'
       WHEN right(left(ae.Department,6),2) = 'BH' THEN 'Barnet'
       WHEN right(left(ae.Department,6),2) = 'CF' THEN 'Chase Farm'
       WHEN left(ae.[Organisation_Code_(Code_of_Provider)],3) = 'RAL' THEN 'Royal Free'
       ELSE 'Other' END															AS 'Hospital Site'

,1 as 'Attendances'

,case 
when Arrival_Date < @pothead_cutoff then
(case when b.Breach_ID is null then 0 else 1 end)
when Arrival_Date >= @pothead_cutoff then
(case when datediff(MI,Arrival_Date+ae.Arrival_Time,(case when Departure_Time <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end)+Departure_Time) >= 240 then 1 else null end) 
end																				AS 'Breaches'

,case 
when Arrival_Date < @pothead_cutoff then
(case when b.Breach_ID is null then null else b.Breach_Reason end)
when Arrival_Date >= @pothead_cutoff then
(case when datediff(MI,Arrival_Date+ae.Arrival_Time,(case when Departure_Time <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end)+Departure_Time) >= 240 then p.[ED Breach Reason] else null end) 
end																				AS 'Breach_Reason'

,case 
when Arrival_Date < @pothead_cutoff then
(case when b.Breach_ID is null then null else b.Breach_Reason end)
when Arrival_Date >= @pothead_cutoff then
(case when datediff(MI,Arrival_Date+ae.Arrival_Time,(case when Departure_Time <= ae.Arrival_Time then Arrival_Date+1 else Arrival_Date end)+Departure_Time) >= 240 then LEFT(p.[ED Breach Reason],LEN(p.[ED Breach Reason])-(LEN(p.[ED Breach Reason])-(case when CHARINDEX(',',p.[ED Breach Reason]) = 0 then LEN(p.[ED Breach Reason]) else CHARINDEX(',',p.[ED Breach Reason])-1 end))) else null end) 
end																				AS 'First_Breach_Reason'

,sp.[Event_Display]																AS 'Ref Specialty'


FROM RF_Performance.dbo.RF_Performance_AE_Main as ae

left join
(select distinct * from DIV_UC.dbo.RFL_AEbuildingtemp) aeb
on aeb.[A&E Attendance Number] = ae.ATTENDANCE_NUMBER

left join Ardentia_Healthware_64_Reference.dbo.RF_Dates Dates
ON ae.Arrival_Date = Dates.Date

left outer join RF_Performance.dbo.RF_PowerformDataAAE as p
on ae.CDS_Unique_Identifier = p.CDSUniqueIdentifier_AE

left join [RF_PIEDWOut].[PIEDWRFL].[EDDischargeInformation] p2
on ae.Attendance_Number = p2.FinNo

left join [DIV_UC].[dbo].[Potatohead_AE_Breaches_All] b
on case when left(b.Breach_Reference,1) ='0'
              then right(b.Breach_Reference,len(b.Breach_Reference-1))
              else b.Breach_Reference end = case when left(ae.Local_Patient_ID,1) ='0'
                                                                           then right(ae.Local_Patient_ID,len(ae.Local_Patient_ID-1))
                                                                           else ae.Local_Patient_ID end
and cast(b.Breach_Date as date) = cast(ae.Arrival_Date as date)
and cast(b.Arrival_Time as time) = cast(ae.Arrival_Time as time)

left join 
(
select *
from (
SELECT
[A&E_Attendance_Number]
,[A&E_Arrival_DateTime]
,[Request_DateTime]
,[Onset_DateTime]
,[Event_Display]
,[Update_Date]
,ROW_NUMBER()over(partition by [A&E_Attendance_Number] order by [Request_DateTime] asc) as Orderby

FROM
[DIV_UC].[dbo].[RFL_ED_Ref_Spec]

where
Event_Display like '%ref%'
)x 
where Orderby = 1
) sp
on sp.[A&E_Attendance_Number] = ae.Attendance_Number


where
Arrival_Date between @start and @end
and
Attendance_Category <> '2'

order by 2


