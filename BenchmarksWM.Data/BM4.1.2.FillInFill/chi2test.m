function [h, chi2, p] = chi2test(x)

chance = sum(x).*ones(size(x))./numel(x);

chi2 = sum(((x-chance).^2)./chance);
p = 1 - chi2cdf(chi2, numel(x)-1);
h = p<.05;