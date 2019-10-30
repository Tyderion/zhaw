function [s] = distance(p,q)


dif = p - q;
dif_squared = dif.^2;
s = sqrt(sum(dif_squared));

%alternative: pdist

end

