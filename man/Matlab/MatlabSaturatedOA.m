% The file use function SaturatedOA to generate an saturated othogonal array OA(N,s^n,s,2) with N=s^k and
% n=(N-1)/(s-1), whose columns are arranged in Yates order.
load('input.mat')
A=SaturatedOA(s,k)
save('OA','A')