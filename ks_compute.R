# om
ks_compute=function(score,response)
{
	library(matlab)
#sr_score=sort(score,decreasing=TRUE);
frame=data.frame(score,response);

sr_frame=frame[with(frame,order(-score)),]

## Divide in 10 deciles
group=10;
## Total number of records
totalnum=length(score);
## get the bin size ~ total # of records divided into the 10 deciles
binsize=round(totalnum/group);
maxscore=NULL;
minscore=NULL;
predresp=NULL;
actresp=NULL;
cumacc=NULL;
GRPNAME=NULL;
cumrespr=NULL;
cumresp=NULL;
# Total number of +ve responses
resp_cnt=sum(response);
nonresp_cnt=totalnum-resp_cnt;
pctresp=NULL;
KSOPT=NULL;
GOFOPT=NULL;
cumresp_s=0;
GOF=0;
for (i in 1:group)
{
startidx=(i-1)*binsize+1;
endidx=i*binsize;
if (endidx>totalnum)
{
endidx=totalnum;
}
GRPNAME[i]=i;
cumacc[i]=endidx;
## The first score in the group
maxscore[i]=sr_frame[startidx,1];
## The last score in the group
minscore[i]=sr_frame[endidx,1];
## predicted responses = Average score in each deciles
predresp[i]=mean(sr_frame[startidx:endidx,1])*100;
## actual responses = Average responses in each deciles
actresp[i]=mean(sr_frame[startidx:endidx,2])*100;
#maxscore[i]=sr_score[startidx];
#minscore[i]=sr_score[endidx];
#minscore[i]=min(sr_score[startidx:endidx]);
#maxscore[i]=max(sr_score[startidx:endidx]);
#index=find(score>minscore[i] & score<=maxscore[i]);
#
#predresp[i]=mean(sr_score[startidx:endidx])*100;
#actresp[i]=mean(response[index])*100;

#predresp[i]=mean(score[index])*100;
#actresp[i]=mean(response[index])*100;
cumresp_s=cumresp_s+actresp[i];

## Cumulative response rate in subsequent buckets .i.e. Cumulative_Sum(actresp)/i
cumrespr[i]=cumresp_s/i;

## Cumulative response captured in subsequent buckets
cumresp[i]=round(cumacc[i]*cumrespr[i]/100);

## % of response captured in subsequent buckets. the toal should sum up to 100% responders
pctresp[i]=round(cumresp[i]*100/resp_cnt,2);

## Cumulative non responders in subsequent buckets
cumnonresp=round((cumacc[i]-cumresp[i])*100/nonresp_cnt,2);
KSOPT[i]=abs(pctresp[i]-cumnonresp)
L4=binsize*actresp[i]/100;
G4=predresp[i]/100;
GOF=GOF+(((L4-binsize*G4)^2)/(binsize*G4*(1-G4)));
GOFOPT[i]=GOF;
}

KS_table=data.frame(GRPNAME,cumacc,predresp,actresp,minscore,maxscore,cumrespr,cumresp,pctresp,KSOPT,GOFOPT);
KS_Score=max(KSOPT)
#print(KS_table)
#print(KS_Score)
return(KS_table);
}