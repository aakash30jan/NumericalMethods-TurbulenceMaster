x=[  1.00000000e+01,   1.00000000e+02,   1.00000000e+03, 1.00000000e+04,   1.00000000e+05]
y=[  1.97327158e-07,   7.92858219e-06,   8.90995957e-05, 9.01326232e-04,   9.02364586e-03]
y=[ 0.07326876,  0.07325522,  0.07307861,  0.07130311,  0.05354711]

x=[  1.00000000e+01,   1.00000000e+02,   1.00000000e+03, 1.00000000e+04,   1.00000000e+05]
y=[  1.35165092e-08,   5.35637785e-07,   5.99727266e-06,       6.06433320e-05,   6.07106955e-04]
y=[  8.50368692e-07,   8.53467218e-07,   8.94157719e-07,        1.30330505e-06,   5.39502539e-06]


xx=x.^2;
xxx=x.^3;
xxxx=x.^4;
xy=x.*y;
xxy=x.*x.*y;
n1=length(x);
a=[sum(x) sum(x) n1;sum(xxx) sum(xx) sum(x); sum(xxxx) sum(xxx) sum(xx)];
b=[sum(y);sum(xy); sum(xxy)];
[m,n]=size(a);
for i=1:1:n-1
    for k=i+1:1:n
        factor=a(k,i)/a(i,i);
        for j=1:1:n
            a(k,j)=a(k,j)-factor*a(i,j);
        end
        b(k)=b(k)-factor*b(i);
    end
end
disp(' Final Augumented Matrix is:');
disp([a,b]);
for i=n:-1:1
    temp=b(i);
    for j=i+1:1:n
        temp=temp-a(i,j)*x(i);
    end
    x(i)=temp/a(i,i);
end
fprintf('y=%fx2  + %fx + %f\n', x(1), x(2), x(3));
