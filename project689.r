# Import the data
raw_data <- read.table("drug_consumption.data", sep=",", header=FALSE)

# Using these creterions: Age, Gender, Education, Psycological Scores (7 scores in total)
# Using these labels: Cannabis, Coke, Ecstasy, Heroin, Ketamine
data_for_use <- raw_data[,c(2,3,4,7,8,9,10,11,12,13,19,21,23,24,25)]
colnames(data_for_use)<- c('Age','Gender','Education','Nscore',
                           'Escore','Oscore','Ascore','Cscore','Impulsive',
                           'Sensation','Cannabis','Cocaine','Ecstasy','Heroin',
                           'Ketamine')
# Convert numerical to factors 
#                                          0              1           2            3            4
# For Neo-FFI scores, set these levels:   <=20          , 20~30     , 30~40      , 40~50      , >=50(very high),
# For Impulsive scores, set:              < -1          , -1~0,     , 0~1        ,  1~2       , >2
# For Sensation scores, set:              < -1.5        , -1.5~-0.5 , -0.5~0.5   , 0.5~ 1.5,  , >1.5
data_for_use$Nscore <- cut(data_for_use$Nscore, breaks=c(-Inf, -1.7, -0.5, 0.5, 1.5, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Escore <- cut(data_for_use$Escore, breaks=c(-Inf, -2.5, -1.3, 0.1, 1.6, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Oscore <- cut(data_for_use$Oscore, breaks=c(-Inf, -4, -2.1, -0.8, 0.6, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Ascore <- cut(data_for_use$Ascore, breaks=c(-Inf, -3, -1.8, -0.4, 1.2, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Cscore <- cut(data_for_use$Cscore, breaks=c(-Inf, -2.8, -1.5, -0.2, 1.35, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Impulsive <- cut(data_for_use$Impulsive, breaks=c(-Inf, -1, -0, 1, 2, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Sensation <- cut(data_for_use$Sensation, breaks=c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), 
                           labels=c(0,1,2,3,4))

#                                              0      1      2      3       4       5
# For Age Groups the attributes are converted:  18-24, 25-34, 35-44, 45-54, 55-64,  65+  
data_for_use$Age <- cut(data_for_use$Age, breaks=c(-Inf, -0.5, 0, 0.5, 1.5, 2,Inf), 
                           labels=c(0,1,2,3,4,5))

# For Education the attributes are converted:  0: Left school before 18
#                                              1: Some college or university, no certificate or degree
#                                              2: Professional certificate/ diploma
#                                              3: University degree
#                                              4: Master and above 
data_for_use$Education <- cut(data_for_use$Education, breaks=c(-Inf, -1, -0.5, 0, 0.5, Inf), 
                           labels=c(0,1,2,3,4))
data_for_use$Gender <- cut(data_for_use$Gender, breaks=0, 
                           labels=c('M','F'))

# For each kind of drug, cut them into 3 categories: low mid high
for(i in 11:15)
{
    data_for_use[,i] <- recode(data_for_use[,i], "c('CL1','CL2','CL3')='1';c('CL4','CL5','CL6')='2'; else='0'")
}
save(data_for_use,file="formatted_data.Rdata")

###################################################################################################
# begin identifying the Pmap
###################################################################################################
library(infotheo)

# Predicting the skeleton using conditonal mutual information
skeleton_condmi <- function(datas, threshold)
{
    ndim <- length(datas)
    witness <- array(0,c(ndim,ndim,ndim))
    skeleton <- array(1,c(ndim,ndim))
    colnames(skeleton) <- colnames(datas)
    row.names(skeleton)<- colnames(datas)
    for (i in 1:ndim)
    {
        for (j in 1:ndim)
        {
            if (i!=j)
            {
                for (k in 1:ndim)
                    {
                        if((i!=k)&&(j!=k))
                        {
                            witness[i,j,k] <- condinformation(datas[,i],datas[,j],datas[,k])< threshold
                        }

                    } 
                if(sum(witness[i,j,])>0)
                {skeleton[i,j]<-0}
            }            
        }
    }
    diag(skeleton) <- 0
    return(list(skeleton,witness))
}
pmap_decision <- function(skeleton,witness,predictor_attribute,output_attribute)
{
    directed <- skeleton
    dim_pred <- length(predictor_attribute)
    dim_out <- length(output_attribute)
    ndim <- dim_pred+dim_out
    immo_cnt <- 2
    for(i in 1:dim_pred)
    {
        point_line <- 0
        for(j in 1:dim_pred)
        {
            if (skeleton[i,j]==1)
            {
                point_line <- point_line+1
            }
        }
        if (point_line >= 2)
        {
            for(j in 1:dim_pred)
            {
                for(k in 1:dim_pred)
                {
                    if ((i>=j)&&(i>=k)&&(j!=k))
                    {
                        if((skeleton[i,j]==1)&&(skeleton[i,k]==1)&&(skeleton[j,k]==0))
                        {immo_cnt <- 1}
                        if (witness[j,k,i]==1)
                        {immp_cnt <- 0}
                        if (immo_cnt == 1)
                        {
                            skeleton[i,j] <- 0
                            skeleton[i,k] <- 0
                            directed[i,j] <- 0
                            directed[i,k] <- 0
                        }
                        if (immo_cnt == 0)
                        {
                            if(directed[i,j]== 0)
                            {directed[k,i] <- 0}
                            if(directed[i,k] == 0)
                            {directed[j,i] <- 0}
                        }
                    }
                        

                }

            }
        }
        for(k in output_attribute)
        {
            directed[i,k] <- skeleton[i,k]
            directed[k,i] <- 0
        }
    }
    for(i in predictor_attribute)
    {
        for(j in predictor_attribute)
        {
            if((directed[i,j]==directed[j,i])&&(directed[i,j]==1))
            {
                if(i<j)
                {
                    directed[j,i] <- 0
                }
            }
        }
    }
    # in this application, age, gender attributes should be independent
    if((directed[1,2]==1)||(directed[2,1]==1))
    {
        directed[1,2] <- 0
        directed[2,1] <- 0
    }
    return(directed)
}

structure_learn <- function(datas, threshold)
{
    sklrn <- skeleton_condmi(datas,threshold);
    skeleton <- sklrn[[1]]
    witness <- sklrn[[2]]
    skeleton[c(11:15),c(11:15)]<- array(0,c(5,5))
    learned_structure <- pmap_decision(skeleton,witness,c(1:10),c(11:15))
    return (learned_structure)
}

# Using a threshold 0.018, we found a structure that looks well...
gx <- tructure_learn(data_for_use,0.018)
# graph output
graphout <- graph_from_adjacency_matrix(gx,mode = "directed")
write_graph(graphout, "graphout.gml",format="gml")

# Using MLE to learn coefficients for prediction
coef_learn <- function(training_data)
{
    all_lvls <- list()
    for(i in 1:15)
    {
        all_lvls[[i]] <- levels(training_data[,i])
    }
    # P11|1,2,3,6,8,10
    attrs <- c(11,1,2,3,6,8,10)
    size <- array()
    for(i in 1:length(attrs))
    {
        size[i] <- length(all_lvls[[attrs[i]]])
    }
    param11 <- array(0,size)
    for(i in 1:size[1])
    {
        for(j in 1:size[2]) 
        {
            for(k in 1:size[3])
            {
                for(l in 1:size[4])
                {
                    for(m in 1:size[5])
                    {
                        for(n in 1:size[6])
                        {
                            for(o in 1:size[7])
                            {
                                m1 <- sum(as.numeric((training_data[,11]==(all_lvls[[11]][i]))
                                            &(training_data[,1]==(all_lvls[[1]][j]))
                                            &(training_data[,2]==(all_lvls[[2]][k]))
                                            &(training_data[,3]==(all_lvls[[3]][l]))
                                            &(training_data[,6]==(all_lvls[[6]][m]))
                                            &(training_data[,8]==(all_lvls[[8]][n]))
                                            &(training_data[,10]==(all_lvls[[10]][o]))))
                                m2 <- sum(as.numeric((training_data[,1]==(all_lvls[[1]][j]))
                                            &(training_data[,2]==(all_lvls[[2]][k]))
                                            &(training_data[,3]==(all_lvls[[3]][l]))
                                            &(training_data[,6]==(all_lvls[[6]][m]))
                                            &(training_data[,8]==(all_lvls[[8]][n]))
                                            &(training_data[,10]==(all_lvls[[10]][o]))))
                                param11[i,j,k,l,m,n,o] <- m1/m2
                                if((m1==0)&&(m2==0))
                                {param11[i,j,k,l,m,n,o] <- 0}
                            }

                        }
                    }
                }
            }       
        }
    }

    # P12|1,10
    attrs <- c(12,1,10)
    size <- array()
    for(i in 1:length(attrs))
    {
        size[i] <- length(all_lvls[[attrs[i]]])
    }
    param12 <- array(0,size)
    for(i in 1:size[1])
    {
        for(j in 1:size[2]) 
        {
            for(k in 1:size[3])
            {

                    m1 <- sum(as.numeric((training_data[,12]==(all_lvls[[12]][i]))
                                &(training_data[,1]==(all_lvls[[1]][j]))
                                &(training_data[,10]==(all_lvls[[10]][k]))))
                    m2 <- sum(as.numeric((training_data[,1]==(all_lvls[[1]][j]))
                                &(training_data[,10]==(all_lvls[[10]][k]))))
                    param12[i,j,k] <- m1/m2
                    if((m1==0)&&(m2==0))
                    {param12[i,j,k] <- 0}
            }       
        }
    }

    # P13|1,10
    attrs <- c(13,1,10)
    size <- array()
    for(i in 1:length(attrs))
    {
        size[i] <- length(all_lvls[[attrs[i]]])
    }
    param13 <- array(0,size)
    for(i in 1:size[1])
    {
        for(j in 1:size[2]) 
        {
            for(k in 1:size[3])
            {

                    m1 <- sum(as.numeric((training_data[,13]==(all_lvls[[13]][i]))
                                &(training_data[,1]==(all_lvls[[1]][j]))
                                &(training_data[,10]==(all_lvls[[10]][k]))))
                    m2 <- sum(as.numeric((training_data[,1]==(all_lvls[[1]][j]))
                                &(training_data[,10]==(all_lvls[[10]][k]))))
                    param13[i,j,k] <- m1/m2
                    if((m1==0)&&(m2==0))
                    {param13[i,j,k] <- 0}
            }       
        }
    }

    # P14
    lvls <- levels(training_data[,14])
    size <- length(lvls)
    param14 <- array(0,size)
    for(i in 1:size)
    {
        param14[i] <- sum(as.numeric(training_data[,14]==lvls[i]))/length((training_data[,14]))
    }

    # P15|1
    attrs <- c(15,1)
    size <- array()
    for(i in 1:length(attrs))
    {
        size[i] <- length(all_lvls[[attrs[i]]])
    }
    param15 <- array(0,size)
    for(i in 1:size[1])
    {
        for(j in 1:size[2])
            {
                m1 <- sum(as.numeric((training_data[,15]==(all_lvls[[15]][i]))
                                    &(training_data[,1]==(all_lvls[[1]][j]))))
                m2 <- sum(as.numeric(training_data[,1]==(all_lvls[[1]][j])))
                param15[i,j] <- m1/m2
                if((m1==0)&&(m2==0))
                {param15[i,j] <- 0}
            }
    }
    return(list(param11,param12,param13,param14,param15))
}

# Using the learned coefficients for prediction
all_lvls <- list()
for(i in 1:15)
{
    all_lvls[[i]] <- levels(data_for_use[,i])
}
pred <- function(coef_learn,test_data,all_lvls)
{
    #11
    attrs <- c(1,2,3,6,8,10)
    pos <- rep(0,length(attrs))
    for(i in 1:length(attrs))
    {
        pos[i] <- which(all_lvls[[attrs[i]]]==unlist(test_data[attrs[i]]))
    }
    pred_11 <- coef_learn[[1]][,pos[1],pos[2],pos[3],pos[4],pos[5],pos[6]]

    #12
    attrs <- c(1,10)
    pos <- rep(0,length(attrs))
    for(i in 1:length(attrs))
    {
        pos[i] <- which(all_lvls[[attrs[i]]]==unlist(test_data[attrs[i]]))
    }
    pred_12 <- coef_learn[[2]][,pos[1],pos[2]]

    #13
    attrs <- c(1,10)
    pos <- rep(0,length(attrs))
    for(i in 1:length(attrs))
    {
        pos[i] <- which(all_lvls[[attrs[i]]]==unlist(test_data[attrs[i]]))
    }
    pred_13 <- coef_learn[[3]][,pos[1],pos[2]]

    #15
    pos1 <- which(all_lvls[[1]]==unlist(test_data[1]))
    pred_15 <- coef_learn[[5]][,pos1]
    return(c(all_lvls[[11]][which(pred_11==max(pred_11))[1]],
             all_lvls[[12]][which(pred_12==max(pred_12))[1]],
             all_lvls[[13]][which(pred_13==max(pred_13))[1]],
             all_lvls[[15]][which(pred_15==max(pred_15))[1]]))
}

# Comparing with Naive Bayes
library(naivebayes)

pred_nb <- function(training_data,test_data)
{
    require(naivebayes)
    nbfit_11 <- naive_bayes(training_data[,c(1:10)],training_data[,11]) 
    nbfit_12 <- naive_bayes(training_data[,c(1:10)],training_data[,12])
    nbfit_13 <- naive_bayes(training_data[,c(1:10)],training_data[,13])
    nbfit_15 <- naive_bayes(training_data[,c(1:10)],training_data[,15])
    return(array(c(predict(nbfit_11,test_data[,c(1:10)],type="class"),
                predict(nbfit_12,test_data[,c(1:10)],type="class"),
                predict(nbfit_13,test_data[,c(1:10)],type="class"),
                predict(nbfit_15,test_data[,c(1:10)],type="class")),c(dim(test_data)[1],4))-1)
}

# random test
err_pgm <- array(0,c(10,4))
err_nb <- array(0,c(10,4))
library(caret)
for(q in 1:10)
{
    set.seed(q)
    n <- 1885
    shuffled_df <- data_for_use[sample(n), ]
    train_indices <- 1:round(0.9 * n)
    train_data <- shuffled_df[train_indices, ]
    test_indices <- (round(0.9 * n) + 1):n
    test_data <- shuffled_df[test_indices, ]
    coefs_pgm <- coef_learn(train_data)
    result_pgm <- array(0,c(dim(test_data)[1],4))
    for(r in 1:dim(test_data)[1])
    {
        result_pgm[r,]<- pred(coefs_pgm,test_data[r,],all_lvls)
    }
    result_nb <- pred_nb(train_data,test_data)
    test_Y <- test_data[,c(11,12,13,15)]
    for(s in 1:4)
    {
        err_pgm[q,s] <- sum(as.numeric(test_Y[,s]==result_pgm[,s]))/(dim(test_data)[1])
        err_nb[q,s] <- sum(as.numeric(test_Y[,s]==result_nb[,s]))/(dim(test_data)[1])
    }
}

# plot the standard deviation errors of the accuracy
attach(mtcars)
par(mfrow=c(2,2))
plot(mean(err_pgm[,1]),rowMeans(TPR_cor_N),main="ROC curve for correlation",type="l",col='blue',pch=1)
plot(rowMeans(FPR_pcor_N),rowMeans(TPR_pcor_N),main="ROC curve for partial correlation",type="l",col='blue',pch=2)
plot(rowMeans(FPR_mi_N),rowMeans(TPR_mi_N),main="ROC curve for mutual information",type="l",col='blue',pch=3)

err_means <- c(mean(err_pgm[,1]),mean(err_pgm[,2]),mean(err_pgm[,3]),mean(err_pgm[,4]))
err_names <- c("Cannabis", "Cocaine", "Ecstasy","Ketamine")
err_stde <- c(sd(err_pgm[,1]),sd(err_pgm[,2]),sd(err_pgm[,3]),sd(err_pgm[,4]))
plotTop <- max(err_means+err_stde*2)
barCenters <- barplot(err_means, names.arg=err_names, col="gray", las=1, ylim=c(0,plotTop))
segments(barCenters, err_means-err_stde*2, barCenters, err_means+err_stde*2, lwd=2)


# depricated functions
my_condmi4 <- function(x,y,a,b)
{
    
    hxab <- 0
    hyab <- 0
    hxyab <- 0
    hab <- 0
    for(k in 1:length(levels(a)))
    {
        for(l in 1:length(levels(b)))
        {
            nab <- sum(as.numeric((a==(levels(a)[k]))&(b==(levels(b)[l]))))
            plgab <- nab/length(x)*log(nab/length(x))
            if(is.nan(plgab)||is.na(plgab))
            {plgab <- 0}
            hab <- hab- plgab
            for (i in 1:length(levels(x)))
            {
                nxab <- sum(as.numeric((x==(levels(x)[i]))&(a==(levels(a)[k]))&(b==(levels(b)[l]))))
                plgxab <- nxab/length(x)*log(nxab/length(x))
                if(is.nan(plgxab)||is.na(plgxab))
                {plgxab <- 0}
                hxab <- hxab- plgxab
            }
            for (j in 1:length(levels(y)))
            {
                nyab <- sum(as.numeric((y==(levels(y)[j]))&(a==(levels(a)[k]))&(b==(levels(b)[l]))))
                plgyab <- nyab/length(y)*log(nyab/length(y))
                if(is.nan(plgyab)||is.na(plgyab))
                {plgyab <- 0}
                hyab <- hyab- plgyab
            }

        }
    }
    for(i in 1:length(levels(x)))
    {
        for(j in 1:length(levels(y)))
        {
            for(k in 1:length(levels(a)))
            {
                for(l in 1:length(levels(b)))
                {
                    nxyab <- sum(as.numeric((x==(levels(x)[i]))&(y==(levels(y)[j]))&
                                            (a==(levels(a)[k]))&(b==(levels(b)[l]))))
                    plgxyab <- nxyab/length(x)*log(nxyab/length(x))
                    if(is.nan(plgxyab)||is.na(plgxyab))
                    {plgxyab <- 0}
                    hxyab <- hxyab- plgxyab
                }
            }
        }
    }
    return(hxab+hyab-hxyab-hab)
}
my_condmi2 <- function(x,y,z)
{
    hxz <- 0
    hyz <- 0
    hxyz <- 0
    hz <- 0
    for(i in 1:length(levels(z)))
    {
        nz <- sum(as.numeric(z==levels(z)[i]))
        plgz <- nz/length(z)*log(nz/length(z))
        hz <- hz-plgz
        for(j in 1:length(levels(x)))
        {
            nxz <- sum(as.numeric((x==levels(x)[j])&(z==levels(z)[i])))
            plgxz <- nxz/length(x)*log(nxz/length(x))
            if(is.nan(plgxz)||is.na(plgxz))
            {plgxz <- 0}
            hxz <- hxz- plgxz
        }
        for(k in 1:length(levels(y)))
        {
            nyz <- sum(as.numeric((y==levels(y)[k])&(z==levels(z)[i])))
            plgyz <- nyz/length(y)*log(nyz/length(y))
            if(is.nan(plgyz)||is.na(plgyz))
            {plgyz <- 0}
            hyz <- hyz- plgyz
        }
    }
    for(i in 1:length(levels(x)))
    {
        for(j in 1:length(levels(y)))
        {
            for(k in 1:length(levels(z)))
            {
                nxyz <- sum(as.numeric((x==levels(x)[i])&(y==levels(y)[j])&(z==levels(z)[k])))
                plgxyz <- nxyz/length(x)*log(nxyz/length(x))
                if(is.nan(plgxyz)||is.na(plgxyz))
                {plgxyz <- 0}
                hxyz <- hxyz- plgxyz
            }
        }
    }
    return(hxz+hyz-hxyz-hz)
}


