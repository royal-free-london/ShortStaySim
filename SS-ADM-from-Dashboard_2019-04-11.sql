select
Local_Patient_ID
,[Start_Date_(Hospital_Provider_Spell)]
,[Start_DateTime_(Hospital_Provider_Spell)]
,Hospital_Provider_Spell_Number
,case 
when cast([Start_DateTime_(Hospital_Provider_Spell)] as time) between '08:00:00' and '13:59:59' then '08-14'
when cast([Start_DateTime_(Hospital_Provider_Spell)] as time) between '14:00:00' and '19:59:59' then '14-20'
when cast([Start_DateTime_(Hospital_Provider_Spell)] as time) between '20:00:00' and '23:59:59' then '20-02'
when cast([Start_DateTime_(Hospital_Provider_Spell)] as time) between '00:00:00' and '01:59:59' then '20-02'
when cast([Start_DateTime_(Hospital_Provider_Spell)] as time) between '02:00:00' and '07:59:59' then '02-08'
end as 'Time Band'

,Specialty_Desc as Specialty

,Ward_Code_at_Start_Episode
,Ward_Code_at_End_Episode
,case 
when ((Ward_Code_at_End_Episode like '%aau%') or (Ward_Code_at_End_Episode like '%qv%')) THEN 'AAU'
when (Ward_Code_at_End_Episode like '%8 north%') THEN 'MAU'
else 'Spec_Ward' end as 'Spec_Ward_Flag'
,case 
when (Ward_Code_at_Start_Episode like '%aau%' or Ward_Code_at_Start_Episode like '%qv%') then 'AAU'
when Ward_Code_at_Start_Episode like '%8 north%' then 'MAU'
else 'Other'
end as 'AAU/MAU'

,[Site_Code_(of_Treatment)_(at_start_of_episode)]

,[Admission_Method_(Hospital_Provider_Spell)]
,[Admission_Method_(Hospital_Provider_Spell)_Desc]
,case when [Admission_Method_(Hospital_Provider_Spell)] like '21' then 'Non-Elective (ED)' else 'Other' end as 'Admission From'

,TimeBetweenReadmissions
,case when TimeBetweenReadmissions <= 7 then '7 day Re-admission' end as '7 Day Re-admission'
,case when TimeBetweenReadmissions <= 30 then '30 day Re-admission' end as '30 Day Re-admission'
,[HQU16-n - Emergency Readmissions 30 Days (All)]

,DATEDIFF(dd,[Start_Date_(Hospital_Provider_Spell)],case when [Discharge_Date_(Hospital_Provider_Spell)] is null then getdate() else [Discharge_Date_(Hospital_Provider_Spell)] end) as LOS
,d.WeekBeginning

,case when AgeonAdmission >= 18 then 'Adult' else 'Paed' end as 'Adult_Flag'

,Specialty_Desc

from
RF_Performance.dbo.RF_Performance_APC_Main a

left join Ardentia_Healthware_64_Reference.dbo.RF_Dates d
on a.[Start_Date_(Hospital_Provider_Spell)] = d.Date

where
--[Site_Code_(of_Treatment)_(at_start_of_episode)] = 'ral01'
--and
--(Ward_Code_at_Start_Episode like '%aau%' or Ward_Code_at_Start_Episode like '%qv%' or Ward_Code_at_Start_Episode like '%8 north%')
--and
[Start_Date_(Hospital_Provider_Spell)] between '20150326' and DATEADD(WEEK,0,getdate())-DATEPART(WEEKDAY,getdate())
and
Episode_Number in ('1','01')
--and
--(Ward_Code_at_Start_Episode like '%aau%' or Ward_Code_at_Start_Episode like '%qv%' or Ward_Code_at_Start_Episode like '%8 north%')

order by 2 desc