% Define dependent probobility & structure
C_AB = [-0.4,0.3;0.4,-0.3];
D_C = [-0.4,0.35];
E_C = [0.25,-0.25];
G_DE = [-0.25,0.25;0.35,0.4];
F_DG = [-0.4,0.3;0.4,-0.4];

directed = [
    0,0,1,0,0,0,0;
    0,0,1,0,0,0,0;
    -1,-1,0,1,1,0,0;
    0,0,-1,0,0,1,1;
    0,0,-1,0,0,0,1;
    0,0,0,-1,0,0,-1;
    0,0,0,-1,-1,1,0];

undirected = [
    0,0,1,0,0,0,0;
    0,0,1,0,0,0,0;
    1,1,0,1,1,0,0;
    0,0,1,0,0,1,1;
    0,0,1,0,0,0,1;
    0,0,0,1,0,0,1;
    0,0,0,1,1,1,0];

% Generate n observation
n = 500;
threshold = 0.007;
threshold2 = 0.007;
graph_ske = ones(7,7);
directed_graph = ones(7,7);

A_1 = round(rand(1,n)+0.4);
B_1 = round(rand(1,n)-0.35);

C_1=zeros(1,n);
D_1=zeros(1,n);
E_1=zeros(1,n);
F_1=zeros(1,n);
G_1=zeros(1,n);

rand_1 = rand(1,n); rand_2 = rand(1,n); rand_3 = rand(1,n); rand_4 = rand(1,n); rand_5 = rand(1,n);

for i = 1 : n
    C_1(i) = round( rand_1(i) + C_AB(A_1(i)+1,B_1(i)+1) );
    D_1(i) = round( rand_2(i) + D_C(C_1(i)+1) );
    E_1(i) = round( rand_3(i) + E_C(C_1(i)+1) );
    G_1(i) = round( rand_4(i) + G_DE(D_1(i)+1,E_1(i)+1) );
    F_1(i) = round( rand_5(i) + F_DG(D_1(i)+1,G_1(i)+1) );
end
Rn_1 = [A_1;B_1;C_1;D_1;E_1;F_1;G_1];

p1 = zeros(7,2);
p2 = zeros(7,7,4);
p3 = zeros(7,7,7,8);
p4 = zeros(7,7,7,7,16);
condition_p1 = zeros(7,7,4);
condition_p2 = zeros(7,7,7,8);
condition_p3 = zeros(7,7,7,7,16);
I1 = zeros(7,7,7);
I2 = zeros(7,7,7,7);

for data_n = 1:n
    for i = 1:7
        p1(i,Rn_1(i,data_n)+1) = p1(i,Rn_1(i,data_n)+1) + 1;
        for j = 1:7
            if i~=j
                p2(i,j,Rn_1(i,data_n)*2+Rn_1(j,data_n)+1) = p2(i,j,Rn_1(i,data_n)*2+Rn_1(j,data_n)+1) + 1;
                condition_p1(i,j,Rn_1(i,data_n)*2+Rn_1(j,data_n)+1) = condition_p1(i,j,Rn_1(i,data_n)*2+Rn_1(j,data_n)+1) + 1;
            end
            for k = 1:7
                if (i~=j)&&(i~=k)&&(j~=k)
                    condition_p2(i,j,k,Rn_1(i,data_n)*4+Rn_1(j,data_n)*2+Rn_1(k,data_n)*1+1) = condition_p2(i,j,k,Rn_1(i,data_n)*4+Rn_1(j,data_n)*2+Rn_1(k,data_n)*1+1) + 1;
                end
                for l = 1:7
                    if (i~=j)&&(i~=k)&&(i~=l)&&(j~=k)&&(j~=l)&&(k~=l)
                        condition_p3(i,j,k,l,Rn_1(i,data_n)*8+Rn_1(j,data_n)*4+Rn_1(k,data_n)*2+Rn_1(l,data_n)+1) = condition_p3(i,j,k,l,Rn_1(i,data_n)*8+Rn_1(j,data_n)*4+Rn_1(k,data_n)*2+Rn_1(l,data_n)+1) + 1;
                    end
                end
            end
        end
    end
end

for i = 1:7
    for j = 1:2
        p1(i,j) = p1(i,j)/n;
    end
    for j = 1:7
        for k = 1:4
            if (i~=j)
                p2(i,j,k) = p2(i,j,k)/n; 
                condition_p1(i,j,k) = p2(i,j,k)/p1(j,mod(k-1,2)+1);
            end
        end
        for k = 1:7
            for l = 1:8
                if (i~=j)&&(i~=k)&&(j~=k)
                    p3(i,j,k,l) = condition_p2(i,j,k,l)/n;
                    condition_p2(i,j,k,l) = p3(i,j,k,l)/p1(k,mod(k-1,2)+1);
                end
            end
            for l = 1:7
                for m = 1:16
                    if (i~=j)&&(i~=k)&&(i~=l)&&(j~=k)&&(j~=l)&&(k~=l)
                        p4(i,j,k,l,m) = condition_p3(i,j,k,l,m)/n;
                        condition_p3(i,j,k,l,m) = p4(i,j,k,l,m)/(p2(k,l,mod(m-1,4)+1));
                    end
                end
            end
        end
    end
end

for i = 1:7
    for j = 1:7
        for k =1:7
            for l = 1:8
                if (p3(i,j,k,l)~=0)&&(i~=j)&&(i~=k)&&(j~=k)
                    I1(i,j,k) = I1(i,j,k) + p3(i,j,k,l)*log2((p3(i,j,k,l)*p1(k,mod(l-1,2)+1))/(p2(i,k,floor((l-1)/4)*2+mod(l-1,2)+1)*p2(j,k,(mod(l-1,4)+1))));
                end
            end
            for l = 1:7
                for m = 1:16
                    if (p4(i,j,k,l,m)~=0)&&(i~=j)&&(i~=k)&&(i~=l)&&(j~=k)&&(j~=l)&&(k~=l)
                        I2(i,j,k,l) = I2(i,j,k,l) + p4(i,j,k,l,m)*log2((p4(i,j,k,l,m)*p2(k,l,mod(m-1,4)+1))/(p3(i,k,l,floor((m-1)/8)*4+mod(m-1,4)+1)*p3(j,k,l,(mod(m-1,8)+1))));
                    end
                end
            end
        end
    end
end

for i = 1:7
    for j = 1:7
        line = 1;
        for k = 1:7
            if (abs(I1(i,j,k))<threshold)&&(i~=j)&&(i~=k)&&(j~=k)&&(abs(I1(i,j,k))>0)
                line = 0;
            end
            for l = 1:7
                if (abs(I2(i,j,k,l))<threshold)&&(i~=j)&&(i~=k)&&(i~=l)&&(j~=k)&&(j~=l)&&(k~=l)&&(abs(I2(i,j,k,l))>0)
                    line = 0;
                end
            end
        end
        if line == 0
            graph_ske(i,j) = 0;
            directed_graph(i,j) = 0;
        end
    end
end

FP = 0;
FN = 0;
TP = 0;
TN = 0;
for i = 1:6
    for j = i+1:7
        if (graph_ske(i,j) == 1) && (undirected(i,j) == 0)
            FP = FP + 1;
        end
        if (graph_ske(i,j) == 0) && (undirected(i,j) == 1)
            FN = FN + 1;
        end
        if (graph_ske(i,j) == 1) && (undirected(i,j) == 1)
            TP = TP + 1;
        end
        if (graph_ske(i,j) == 0) && (undirected(i,j) == 0)
            TN = TN + 1;
        end
    end
end
FP1 = FP/21;
FN1 = FN/21; 
TP1 = TP/21;
TN1 = TN/21; 

potential_point = zeros(1,7);
immo_jud = 0;
immo_true1 = 0;
immo_true2 = 0;
for i = 1:7
    point_line = 0;
    for j = 1:7
        if graph_ske(i,j)
            point_line = point_line +1;
        end
    end
    if point_line > 2
        for j = 1:7
            for k = j+1:7
                if graph_ske(i,j)&&graph_ske(i,k)&&(i~=j)&&(i~=k)&&(graph_ske(j,k)==0)
                    immo_true = 0;
                    immo_count = 1;
                    for l = 1:7
                        if ((I2(j,k,i,l)<threshold2)||(I1(j,k,i)<threshold2))&&(i~=l)&&(j~=l)&&(k~=l)
                            immo_count = 0;
                        end
                    end
                    if immo_count
                        directed_graph(i,j) = -1;
                        directed_graph(i,k) = -1;
                        immo_true = 1;
                    end
                    if (immo_count == 0)
                        if (directed_graph(i,j) == -1)
                            directed_graph(k,i) = -1;
                        end
                        if (directed_graph(i,k) == -1)
                            directed_graph(j,i) = -1;
                        end
                    end
                    if immo_true
                        if (i==3)&&(j==1)&&(k==2)
                            immo_true1 = 1;
                        end
                        if (i==7)&&(j==4)&&(k==5)
                            immo_true2 = 1;
                        end
                        immo_jud = immo_jud + 1;
                    end
                end
            end
        end            
    end
end

% P-map number
PDAG_graph = zeros(7,7);
undirect_line = 0;
for i = 1:7
    for j = 1:7
        if (i~=j)&&(directed_graph(i,j)==1)&&(directed_graph(j,i)==1)
            undirect_line = undirect_line +1;
        end
    end
end
pmap_num = 2^undirect_line;

%immorality
immo_TP = immo_true1+immo_true2;
immo_FP = immo_jud-(immo_true1+immo_true2);
immo_TN = 9 - immo_jud + immo_true1+immo_true2;
immo_FN = 2 - (immo_true1+immo_true2);

disp('finish');