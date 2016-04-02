%--------------------------------------PCA-------------------------------------------------------
for i=1:131
    maximum=max(samplematrix(:,i));
    normalized(:,i) = samplematrix(:,i)./maximum;
end
meansample = mean(normalized,1);
zeroedtrain = normalized - repmat(meansample,[500,1]);

for i=1:131
    maximum=max(sampletest(:,i));
    normalized(:,i) = sampletest(:,i)./maximum;
end
meansample = mean(normalized,1);
zeroedtest = normalized - repmat(meansample,[500,1]);
classerror = zeros(2,7);
cov_train = cov(zeroedtrain);
[vectors, values] = eig(cov_train);
vectors = flipud(fliplr(vectors));
values = flipud(fliplr(values));
dimension = [5, 10, 20, 30, 40, 60, 90];
count = 0;
for d=dimension
    classmean = zeros(d,2);
    classmeantest = zeros(d,500,2);
    count = count+1;
    PC = vectors(:,1:d);
    trans_test = PC'*(zeroedtest');
    trans_train = PC'*(zeroedtrain');
    for i=1:2
        numclass(i) = length(find(classifier == (i-1)));
        [index(1:numclass(i),i), ~] = find(classifier == (i-1));
        for j=1:numclass(i)
            classmean(:,i) = classmean(:,i) + trans_train(:,index(j,i));
        end
        classmean(:,i) = classmean(:,i)/numclass(i);
        classcov(1:d,1:d,i) = cov(trans_train(:,index(1:numclass(i),i))');
        Py(i) = numclass(i)/size(zeroedtrain,2);
        classmeantest(:,:,i) = repmat(classmean(:,i),[1,500]);
        BDR(:,i) = sum(-1/2*(trans_test-classmeantest(:,:,i))'*inv(classcov(1:d,1:d,i))*(trans_test-classmeantest(:,:,i)) );
    end    
    maxima = max(BDR,[],2);
    for i=1:500
        if(~isempty(find(BDR(i,:) == maxima(i))))
            [~, classtest(i)] = find(BDR(i,:) == maxima(i));
        end
    end
    for i=1:500
        if(classtest(i) ~= classifiertest(i)+1)
            k = classifiertest(i)+1;
            classerror(k,count) = classerror(k,count) + 1;
        end
    end
    for i=1:2
        classpercenterror(i,count) = classerror(i,count)/numclass(i);
    end
    totalerror(count) = sum(classerror(:,count))/length(sampletest);        
end
%----------------------------------------------------------------------------------------------------

%--------------------------------------SVM-------------------------------------------------------
Cvalues =  [2^-3, 2^-1, 2^1, 2^3, 2^5, 2^7, 2^9, 2^11];
Gvalues = [2^-11, 2^-9, 2^-7, 2^-5, 2^-3, 2^-1];
% cee = Cvalues(1);
% gee = Gvalues(1);
count = 0;
for cee=Cvalues    
    for gee=Gvalues
        count = count+1;    
        myModel = svmtrain(classifier, samplematrix, ['-t 2 -c ', num2str(cee), ' -g ', num2str(gee),'v 2 ']);
        [predicted(:,count), accuracy(:,count), decisionvalues(:,:,count)] = svmpredict(classifiertest, sampletest, myModel);
    end
end
accuracy(2:3,:)=[];
count2 = 0;
for i=1:8
    for j=1:6
        count2 = count2 + 1;
        accuracy2(i,j) = accuracy(1,count2);
    end
end
%----------------------------------------------------------------------------------------------------
%--------------------------------PCA important variables-----------------------------------------------
[vectors,~,values] = pca(zeroed);
cov(zeroed);
% Variance in variable I explained by principal component J
for i=1:131
    var(i) = vectors(i,:)*(values.*vectors(i,:)');
    for j =1:30
        varfrom(i,j) = vectors(i,j)*values(j)*vectors(i,j);
        centvarfrom(i,j) = varfrom(i,j)/var(i);
    end
end
test = sum(centvarfrom,2);
%----------------------------------------------------------------------------------------------------------------

%--------------------------------Formatting for MATLAB------------------------------------------------
tsamplematrix = readtable('samplematrix.csv');
tclassifier = readtable('classifier.csv');
tsampletest = readtable('sampletest.csv');
tclassifiertest = readtable('classifiertest.csv');

tsamplematrix = tsamplematrix(:,2:132);
tsampletest = tsampletest(:,2:132);

tclassifier = tclassifier(:,2);
cclassifier = table2cell(tclassifier);
classifier = cell2mat(cclassifier);

tclassifiertest = tclassifiertest(:,2);
cclassifiertest = table2cell(tclassifiertest);
classifiertest = cell2mat(cclassifiertest);

csamplematrix = table2cell(tsamplematrix);
csampletest = table2cell(tsampletest);

for i=1:500
    for j=1:131
        samplematrix(i,j) = str2num(cell2mat(csamplematrix(i,j)));
    end
end
for i=1:500
    for j=1:131
        sampletest(i,j) = str2num(cell2mat(csampletest(i,j)));
    end
end
%----------------------------------------------------------------------------------------------------------------