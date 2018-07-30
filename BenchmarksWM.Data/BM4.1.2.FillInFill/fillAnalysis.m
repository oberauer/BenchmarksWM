function [transG, transG1, lagCRP, lagCRP1, lagCRPfi, fillin, infill, fillin1, infill1, meanAcc, CRPalln, CRPall1n] = ...
    fillAnalysis(subjvec, recalls)

ll = size(recalls,2);

alag = (-(ll-1):(ll-1));
lagn = length(alag);

Ss = unique(subjvec);

sInd = 1;

for subj=Ss'

    ntrials = sum(subjvec==subj);
    srecalls = recalls(subjvec==subj,:);
    
    tGn = zeros(1,lagn);
    tGd = zeros(1,lagn);
    
    tG1n = zeros(1,lagn);
    tG1d = zeros(1,lagn);
    
    lagCRPn = zeros(1,lagn);
    lagCRPd = zeros(1,lagn);
    
    lagCRP1n = zeros(1,lagn);
    lagCRP1d = zeros(1,lagn);
    
    lagCRPfin = zeros(1,lagn);
    lagCRPfid = zeros(1,lagn);
    
    sinfill = 0;
    sfillin = 0;
    sinfill1 = 0;
    sfillin1 = 0;
    
    % score up accuracy
    refmat = repmat(1:ll,ntrials,1);
    meanAcc(sInd) = mean(mean(refmat==srecalls));
    
    for i=1:ntrials
        if rem(i,1000)==0
            disp(i)
        end
        
        ordErrs = 0; % have there been any order errors so far?
        
        vocab = 1:ll;
        prevlag = 0;
        
        for j=1:ll
            
            if j>1
                if any(srecalls(i,j)==srecalls(i,1:(j-1)))
                    srecalls(i,j)=-1; % exclude repetitions by turning them into omissions
                end
            end
            
            if srecalls(i,j)>0
                
                % -----------LIST LAG
                listlag = j-srecalls(i,j);
                listlagv = j-(1:ll);
                tGn(listlag+ll) = tGn(listlag+ll)+1;
                tGd(listlagv+ll) = tGd(listlagv+ll)+1;
                
                if ordErrs==1
                    tG1n(listlag+ll) = tG1n(listlag+ll)+1;
                    tG1d(listlagv+ll) = tG1d(listlagv+ll)+1;
                end
                
                % -----------FILLIN/INFILL
                if j>1 && prevlag == -1;
                    if listlag == 1
                        sfillin=sfillin+1;
                    elseif listlag==-1
                        sinfill=sinfill+1;
                    end
                    
                    if ordErrs==1
                        if listlag == 1
                            sfillin1=sfillin1+1;
                        elseif listlag==-1
                            sinfill1=sinfill1+1;
                        end
                    end
                end
                
                % -----------ITEM LAG (ie lag crp)
                if j>1 && srecalls(i,j-1)>0
                    itemlag =  srecalls(i,j) - srecalls(i,j-1);
                    itemlagv = vocab - srecalls(i,j-1);
                    lagCRPn(itemlag+ll) = lagCRPn(itemlag+ll)+1;
                    lagCRPd(itemlagv+ll) = lagCRPd(itemlagv+ll)+1;
                    
                    if ordErrs==1
                        lagCRP1n(itemlag+ll) = lagCRP1n(itemlag+ll)+1;
                        lagCRP1d(itemlagv+ll) = lagCRP1d(itemlagv+ll)+1;
                        if prevlag == -1
                            lagCRPfin(itemlag+ll) = lagCRPfin(itemlag+ll)+1;
                            lagCRPfid(itemlagv+ll) = lagCRPfid(itemlagv+ll)+1;
                        end
                        
                    end
                end
      
                % ------------BOOKKEEPING
                if srecalls(i,j)~=j
                    ordErrs = ordErrs+1;
                end
                prevlag = listlag;
                
                %remove item from vocab as per standard lag-CRP analysis
                if srecalls(i,j)>0
                    vocab = setdiff(vocab,srecalls(i,j));
                end
            end
        end % end serpos loop
    end % end trial loop
    
    CRPalln(sInd,:) = lagCRPn;
    CRPall1n(sInd,:) = lagCRP1n;
    transG(sInd,:) = tGn./tGd;
    transG1(sInd,:) = tG1n./tG1d;
    lagCRP(sInd,:) = lagCRPn./lagCRPd;
    lagCRP1(sInd,:) = lagCRP1n./lagCRP1d;
    lagCRPfi(sInd,:) = lagCRPfin./lagCRPfid;
    infill(sInd) = sinfill;
    fillin(sInd) = sfillin;
    infill1(sInd) = sinfill1;
    fillin1(sInd) = sfillin1;
    sInd = sInd +1;
end % end subject loop

lagCRP(:,ll) = NaN;
lagCRP1(:,ll) = NaN;