select 
Hospital_Provider_Spell_Number,Specialty_desc as Specialty
,Ward
,case 
when Ward like '%8n%' then 'MAU'
when (Ward = 'ral aau' or Ward like '%qv%') then 'AAU'
else 'Other'
end as 'AAU/MAU'
,case
when Ward like '%BH%' then 'BARNET HOSPITAL'   
when Ward like '%CF%' then 'CHASE FARM HOSPITAL'
else 'ROYAL FREE HOSPITAL' end as 'Site'
,[MIN DATE TIME] as [Admission Date Time]
,[MAX DATE TIME] as [Discharge Date Time]

,case 
when cast([MIN DATE TIME] as time) between '08:00:00' and '13:59:59' then '08-14'
when cast([MIN DATE TIME] as time) between '14:00:00' and '19:59:59' then '14-20'
when cast([MIN DATE TIME] as time) between '20:00:00' and '23:59:59' then '20-02'
when cast([MIN DATE TIME] as time) between '00:00:00' and '01:59:59' then '20-02'
when cast([MIN DATE TIME] as time) between '02:00:00' and '07:59:59' then '02-08'
end as 'Time Band'


,cast(cast([MAX DATE TIME] as date) as datetime) as Discharge_Date
,d.WeekBeginning
,DATEDIFF ("DD",[MIN DATE TIME],[MAX DATE TIME]) as [LOS (days)]
,DATEDIFF ("HH",[MIN DATE TIME],[MAX DATE TIME]) as [LOS (hours)]

,case when DATEDIFF ("HH",[MIN DATE TIME],[MAX DATE TIME]) >= 72 then '72+' else '<72' end as '72 Hour Flag'
,case when DATEDIFF ("HH",[MIN DATE TIME],[MAX DATE TIME]) >= 24 then '24+' else '<24' end as '24 Hour Flag'


,cast((d.date-day(d.date)+1) as date) as 'MonthBeginning'



from
(
select 
Hospital_Provider_Spell_Number, Specialty_desc,

MIN (CAST (StartDate as datetime) + CAST (StartTime as datetime)) as [MIN DATE TIME],

MAX (CAST (EndDate as datetime) + CAST (EndTime as datetime)) as [MAX DATE TIME],
RFDetailedWardCode as 'Ward'

from Ardentia_Healthware_64_Release.dbo.APC_WardStays WS

inner join RF_Performance.dbo.RF_Performance_APC_Main APC
ON WS.APCJoinKey = APC.apcjoinkey

--where 
--RFDetailedWardCode like '%8n%' or (RFDetailedWardCode = 'ral aau' or RFDetailedWardCode like '%qv%')

group by 
Hospital_Provider_Spell_Number,
RFDetailedWardCode,
Specialty_desc

) x

left join Ardentia_Healthware_64_Reference.dbo.RF_Dates d
ON CAST ([MAX DATE TIME] as date) = d.Date

where [MAX DATE TIME]
between '20180401' and DATEADD(WEEK,0,getdate())-DATEPART(WEEKDAY,getdate())

order by Hospital_provider_spell_number