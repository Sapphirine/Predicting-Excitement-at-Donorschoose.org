# -*- coding: utf-8 -*-
import re
import sys
import pandas as pd
import numpy as np
from datetime import date

  

def get_history(df, file_name, var="teacher_acctid", responses=["y","y2","y3","y4","y5","y6","y7","y8","y9","y10"]):
    df = df.sort([var, "date_posted"])
    df.index = range(len(df))    
    with open(file_name, "wb") as f:
        f.write(",".join(["projectid",var,"date_posted",var+"_days"] + [var+"_"+r+"_prev" for r in responses] + [var+"_cnt",var+"_cnt_train"])+"\n")   
        prev_var = 0
        for i, v in enumerate(df[var]):
            if prev_var != v:
                prev_var = v
                count_vec = [0]*(len(responses)+2)
                days_btw = 9999                
                f.write(",".join([df["projectid"][i],str(v),df["date_posted"][i],str(days_btw)] + [str(count) for count in count_vec])+"\n")
            else:
                prev_date = date(int(df["year"][i-1]), int(df["month"][i-1]), int(df["day"][i-1]))
                now_date = date(int(df["year"][i]), int(df["month"][i]), int(df["day"][i]))
                days_btw = (now_date - prev_date).days
                f.write(",".join([df["projectid"][i],str(v),df["date_posted"][i],str(days_btw)] + [str(count) for count in count_vec])+"\n")
            for j, r in enumerate(responses):
                count_vec[j] += df[r][i]        
            count_vec[j+1] += 1    
            if df["date_posted"][i] < "2014-01-01":
                count_vec[j+2] += 1
    
if __name__=="__main__":    
    outcomes_df = pd.read_csv( "outcomes.csv")
    projects_df = pd.read_csv( "projects.csv")
    donations_df = pd.read_csv("donations.csv")
    df = pd.merge(projects_df, outcomes_df, how='left', on='projectid')
    df["split"] = "train"
    df["split"][df["date_posted"]<"2010-04-01"] = "none"
    df["split"][df["date_posted"]>="2013-01-01"] = "test"
    df["split"][df["date_posted"]>="2014-01-01"]= "unknow_test"
    df = df[df["split"]!="none"]
    
    df["y"] = 0
    df["y"][df["is_exciting"]=="t"] = 1
    ## y is the value we wanna predict 0/1
    df["y2"] = 0
    df["y2"][df["at_least_1_teacher_referred_donor"]=="t"] = 1
    df["y3"] = 0
    df["y3"][df["great_chat"]=="t"] = 1
    df["y4"] = 0
    df["y4"][df["fully_funded"]=="t"] = 1
    df["y5"] = 0
    df["y5"][df["at_least_1_green_donation"]=="t"] = 1
    df["y6"] = 0
    df["y6"][df["donation_from_thoughtful_donor"]=="t"] = 1
    df["y7"] = 0
    df["y7"][df["three_or_more_non_teacher_referred_donors"]=="t"] = 1
    df["y8"] = 0
    df["y8"][df["one_non_teacher_referred_donor_giving_100_plus"]=="t"] = 1
    df["y9"] = 0
    df["y9"][df["teacher_referred_count"]>=1] = 1
    df["y10"] = 0
    df["y10"][df["non_teacher_referred_count"]>=1]=1
    ## change the categorial variable into dummy variables
    df["year"] = df["date_posted"].apply(lambda x: x.split("-")[0])
    df["month"] = df["date_posted"].apply(lambda x: x.split("-")[1])
    df["day"] = df["date_posted"].apply(lambda x: x.split("-")[2])
    for var in ["teacher_acctid", "schoolid", "school_district", "school_city", "school_county", "school_zip", "school_state"]:
        get_history(df,var+"_history.csv",var,["y","y2","y3","y4","y5","y6","y7","y8","y9","y10"])
    del df
       
    df = pd.merge(projects_df, donations_df, how='left', on='projectid')
    df["y11"] = 1
    df["y12"] = 0
    df["y12"][df["is_teacher_acct"]=="t"] = 1
    df["y13"] = df["donation_total"]
    df["y13"][pd.isnull(df["donation_total"])] = 0
    df2=df.groupby(by=["projectid","date_posted","teacher_acctid","schoolid","school_district","school_city","school_zip"])[["y11","y12","y13"]].sum()
    df2.reset_index(inplace=True)
    df2["year"] = df2["date_posted"].apply(lambda x: x.split("-")[0])
    df2["month"] = df2["date_posted"].apply(lambda x: x.split("-")[1])
    df2["day"] = df2["date_posted"].apply(lambda x: x.split("-")[2])
    for var in ["teacher_acctid", "schoolid", "school_district", "school_city", "school_zip"]:
        get_history(df2,"getDonor"+var+".csv", var, ["y11","y12","y13"])
    del df
