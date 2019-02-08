# MLE Parameters for each P(xi|Pa(xi)) 
all_lvls <- list()
for(i in 1:15)
{
    all_lvls[[i]] <- levels(training_data[,i])
}
# P1
lvls <- levels(training_data[,1])
size <- length(lvls)
param1 <- array(0,size)
for(i in 1:size)
{
    param1[i] <- sum(as.numeric(training_data[,1]==lvls[i]))/length((training_data[,1]))
}
all_params <- list(param1)

# P2
lvls <- levels(training_data[,2])
size <- length(lvls)
param2 <- array(0,size)
for(i in 1:size)
{
    param2[i] <- sum(as.numeric(training_data[,2]==lvls[i]))/length((training_data[,2]))
}
all_params <- c(all_params,list(param2))

# P3|1
attrs <- c(3,1)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param3 <- array(0,size)
for(i in 1:size[1])
{
    for(j in 1:size[2])
        {
            m1 <- sum(as.numeric((training_data[,3]==(all_lvls[[3]][i]))
                                &(training_data[,1]==(all_lvls[[1]][j]))))
            m2 <- sum(as.numeric(training_data[,1]==(all_lvls[[1]][j])))
            param3[i,j] <- m1/m2
        }
}
all_params <- c(all_params,list(param3))

# P4
lvls <- levels(training_data[,4])
size <- length(lvls)
param4 <- array(0,size)
for(i in 1:size)
{
    param4[i] <- sum(as.numeric(training_data[,4]==lvls[i]))/length((training_data[,4]))
}
all_params <- c(all_params,list(param4))

# P5|4
attrs <- c(5,4)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param5 <- array(0,size)
for(i in 1:size[1])
{
    for(j in 1:size[2])
        {
            m1 <- sum(as.numeric((training_data[,5]==(all_lvls[[5]][i]))
                                &(training_data[,4]==(all_lvls[[4]][j]))))
            m2 <- sum(as.numeric(training_data[,4]==(all_lvls[[4]][j])))
            param5[i,j] <- m1/m2
            if((m1==0)&&(m2==0))
            {param5[i,j] <- 0}
        }
}
all_params <- c(all_params,list(param5))

# P6|3,5
attrs <- c(6,3,5)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param6 <- array(0,size)
for(i in 1:size[1])
{
    for(j in 1:size[2]) 
    {
        for(k in 1:size[3])
        {

                m1 <- sum(as.numeric((training_data[,6]==(all_lvls[[6]][i]))
                            &(training_data[,3]==(all_lvls[[3]][j]))
                            &(training_data[,5]==(all_lvls[[5]][k]))))
                m2 <- sum(as.numeric((training_data[,3]==(all_lvls[[3]][j]))
                            &(training_data[,5]==(all_lvls[[5]][k]))))
                param6[i,j,k] <- m1/m2
                if((m1==0)&&(m2==0))
                {param6[i,j,k] <- 0}
        }       
    }
}
all_params <- c(all_params,list(param6))

# P7|4
attrs <- c(7,4)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param7 <- array(0,size)
for(i in 1:size[1])
{
    for(j in 1:size[2])
        {
            m1 <- sum(as.numeric((training_data[,7]==(all_lvls[[7]][i]))
                                &(training_data[,4]==(all_lvls[[4]][j]))))
            m2 <- sum(as.numeric(training_data[,4]==(all_lvls[[4]][j])))
            param7[i,j] <- m1/m2
            if((m1==0)&&(m2==0))
            {param7[i,j] <- 0}
        }
}
all_params <- c(all_params,list(param7))

# P8|1,3,4,5,7
attrs <- c(8,1,3,4,5,7)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param8 <- array(0,size)
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
                        m1 <- sum(as.numeric((training_data[,8]==(all_lvls[[8]][i]))
                                    &(training_data[,1]==(all_lvls[[1]][j]))
                                    &(training_data[,3]==(all_lvls[[3]][k]))
                                    &(training_data[,4]==(all_lvls[[4]][l]))
                                    &(training_data[,5]==(all_lvls[[5]][m]))
                                    &(training_data[,7]==(all_lvls[[7]][n]))))
                        m2 <- sum(as.numeric((training_data[,1]==(all_lvls[[1]][j]))
                                    &(training_data[,3]==(all_lvls[[3]][k]))
                                    &(training_data[,4]==(all_lvls[[4]][l]))
                                    &(training_data[,5]==(all_lvls[[5]][m]))
                                    &(training_data[,7]==(all_lvls[[7]][n]))))
                        param8[i,j,k,l,m,n] <- m1/m2
                        if((m1==0)&&(m2==0))
                        {param8[i,j,k,l,m,n] <- 0}
                    }
                }
            }
        }       
    }
}
all_params <- c(all_params,list(param8))

# P9|3,7,8
attrs <- c(9,3,7,8)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param9 <- array(0,size)
for(i in 1:size[1])
{
    for(j in 1:size[2]) 
    {
        for(k in 1:size[3])
        {
            for(l in 1:size[4])
            {
                m1 <- sum(as.numeric((training_data[,9]==(all_lvls[[9]][i]))
                            &(training_data[,3]==(all_lvls[[3]][j]))
                            &(training_data[,7]==(all_lvls[[7]][k]))
                            &(training_data[,8]==(all_lvls[[8]][l]))))
                m2 <- sum(as.numeric((training_data[,3]==(all_lvls[[3]][j]))
                            &(training_data[,7]==(all_lvls[[7]][k]))
                            &(training_data[,8]==(all_lvls[[8]][l]))))
                param9[i,j,k,l] <- m1/m2
                if((m1==0)&&(m2==0))
                {param9[i,j,k,l] <- 0}
            }
        }       
    }
}
all_params <- c(all_params,list(param9))

# P10|1,3,5,6,7,9
attrs <- c(10,1,3,5,6,7,9)
size <- array()
for(i in 1:length(attrs))
{
    size[i] <- length(all_lvls[[attrs[i]]])
}
param10 <- array(0,size)
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
                            m1 <- sum(as.numeric((training_data[,10]==(all_lvls[[10]][i]))
                                        &(training_data[,1]==(all_lvls[[1]][j]))
                                        &(training_data[,3]==(all_lvls[[3]][k]))
                                        &(training_data[,5]==(all_lvls[[5]][l]))
                                        &(training_data[,6]==(all_lvls[[6]][m]))
                                        &(training_data[,7]==(all_lvls[[7]][n]))
                                        &(training_data[,9]==(all_lvls[[9]][o]))))
                            m2 <- sum(as.numeric((training_data[,1]==(all_lvls[[1]][j]))
                                        &(training_data[,3]==(all_lvls[[3]][k]))
                                        &(training_data[,5]==(all_lvls[[5]][l]))
                                        &(training_data[,6]==(all_lvls[[6]][m]))
                                        &(training_data[,7]==(all_lvls[[7]][n]))
                                        &(training_data[,9]==(all_lvls[[9]][o]))))
                            param10[i,j,k,l,m,n,o] <- m1/m2
                            if((m1==0)&&(m2==0))
                            {param10[i,j,k,l,m,n,o] <- 0}
                        }

                    }
                }
            }
        }       
    }
}
all_params <- c(all_params,list(param10))


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
all_params <- c(all_params,list(param11))

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
all_params <- c(all_params,list(param12))

# P13|1,10
attrs <- c(13,1,10)
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
all_params <- c(all_params,list(param13))

# P14
lvls <- levels(training_data[,14])
size <- length(lvls)
param14 <- array(0,size)
for(i in 1:size)
{
    param14[i] <- sum(as.numeric(training_data[,14]==lvls[i]))/length((training_data[,14]))
}
all_params <- c(all_params,list(param14))

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
all_params <- c(all_params,list(param15))






