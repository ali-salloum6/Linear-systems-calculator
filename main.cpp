#include <iostream>
#include <iomanip>
using namespace std;

class ColumnVector {

private:

    int size=0;
    double *v=NULL;

public:

    ColumnVector() {}

    ColumnVector(int size) {
        this->size=size;
        const int param = size;
        this->v = new double[param];
    }

    ColumnVector(int size,double a[]) {
        this->size=size;
        const int param = size;
        v = new double[param];
        for (int i=0;i<size;i++) {
            this->v[i]=a[i];
        }
    }

    int getSize() const {
        return size;
    }

    double get(int index) const {
        return this->v[index];
    }

    void set(int index,double val) {
        this->v[index]=val;
    }

    ColumnVector& operator=(const ColumnVector &V) {
        this->size=V.getSize();
        v = new double[size];
        for (int i=0;i<size;i++) {
            this->v[i]=V.get(i);
        }
        return *this;
    }

    friend ostream & operator << (ostream &out, const ColumnVector &V){
        for (int i=0;i<V.size;i++) {
            out<<V.v[i]<<'\n';
        }
        return out;
    }

    friend istream & operator >> (istream &in, ColumnVector &V){
        in>>V.size;
        V.v = new double[V.size];
        for (int i=0;i<V.size;i++) {
            in>>V.v[i];
        }
        return in;
    }
};

class Matrix {
protected:
    int rows=0,columns=0;
    double **m=NULL;
public:
    Matrix() {}

    Matrix(int rows,int columns) {
        this->rows=rows;
        this->columns=columns;
        m = new double*[rows];
        for (int i=0;i<rows;i++) {
            m[i] = new double[columns];
        }
        for (int i=0;i<rows;i++) {
            for (int j=0;j<columns;j++) {
                m[i][j]=0;
            }
        }
    }

    Matrix(Matrix A,Matrix B) {
        this->rows=A.getRows();
        this->columns=A.getColumns()+B.getColumns();
        m = new double*[rows];
        for (int i=0;i<rows;i++) {
            m[i] = new double[columns];
        }
        for (int i=0;i<rows;i++) {
            for (int j=0;j<columns;j++) {
                if (j<A.getColumns()) m[i][j]=A.get(i,j);
                else m[i][j]=B.get(i,j-A.getColumns());
            }
        }
    }

    int getRows() const {
        return rows;
    }

    int getColumns() const {
        return columns;
    }

    void set(int i,int j,double val) {
        m[i][j]=val;
    }

    double get(int i,int j) const{
        return m[i][j];
    }

    Matrix operator+(const Matrix &M) {
        if (M.getRows()!=rows || M.getColumns()!=columns) {
            cout<<"Error: the dimensional problem occurred\n";
            return *(new Matrix());
        }
        else {
            Matrix sum(rows,columns);
            for (int i=0;i<rows;i++) {
                for (int j=0;j<columns;j++) {
                    sum.set(i,j,this->m[i][j]+M.get(i,j));
                }
            }
            return sum;
        }
    }

    Matrix operator-(const Matrix &M) {
        if (M.getRows()!=rows || M.getColumns()!=columns) {
            cout<<"Error: the dimensional problem occurred\n";
            return *(new Matrix());
        }
        else {
            Matrix dif(rows,columns);
            for (int i=0;i<rows;i++) {
                for (int j=0;j<columns;j++) {
                    dif.set(i,j,this->m[i][j]-M.get(i,j));
                }
            }
            return dif;
        }
    }

    Matrix operator*(const Matrix &M) {
        if (columns!=M.getRows()) {
            cout<<"Error: the dimensional problem occurred\n";
            return *(new Matrix());
        }
        else {
            Matrix product(rows,M.getColumns());
            for (int i=0;i<rows;i++) {
                for (int j=0;j<M.getColumns();j++) {
                    double sum=0;
                    for (int k=0;k<columns;k++) {
                        sum+=m[i][k]*M.get(k,j);
                    }
                    product.set(i,j,sum);
                }
            }
            return product;
        }
    }

    ColumnVector operator*(const ColumnVector &V) {
        if (columns!=V.getSize()) {
            cout<<"Error: the dimensional problem occurred\n";
            return *(new ColumnVector());
        }
        else {
            ColumnVector product(rows);
            for (int i=0;i<rows;i++) {
                double sum=0;
                for (int k=0;k<columns;k++) {
                    sum+=m[i][k]*V.get(k);
                }
                product.set(i,sum);
            }
            return product;
        }
    }

    Matrix& operator=(const Matrix &M) {
        this->rows=M.getRows();
        this->columns=M.getColumns();
        m = new double*[rows];
        for (int i=0;i<rows;i++) {
            m[i] = new double[columns];
        }
        for (int i=0;i<rows;i++) {
            for (int j=0;j<columns;j++) {
                m[i][j]=M.get(i,j);
            }
        }
        return *this;
    }

    void transpose() {
        double **temp = new double*[columns];
        for (int j=0;j<columns;j++) temp[j]=new double[rows];
        for (int i=0;i<rows;i++) {
            for (int j=0;j<columns;j++) {
                temp[j][i]=m[i][j];
            }
        }
        m = new double*[columns];
        for (int j=0;j<columns;j++) m[j]=new double[rows];
        swap(rows,columns);
        for (int i=0;i<rows;i++) {
            for (int j=0;j<columns;j++) {
                m[i][j]=temp[i][j];
            }
        }
    }

    friend ostream & operator << (ostream &out, const Matrix &M){
        for (int i=0;i<M.rows;i++) {
            for (int j=0;j<M.columns;j++) {
                out<<M.m[i][j]<<' ';
            }
            out<<endl;
        }
        return out;
    }

    friend istream & operator >> (istream &in, Matrix &M){
        in>>M.rows>>M.columns;
        M.m = new double*[M.rows];
        for (int i=0;i<M.rows;i++) {
            M.m[i] = new double[M.columns];
        }
        for (int i=0;i<M.rows;i++) {
            for (int j=0;j<M.columns;j++) {
                in>>M.m[i][j];
            }
        }
        return in;
    }
};

class SquareMatrix : public Matrix {

protected:
    int n;

public:
    SquareMatrix() : Matrix() {}

    SquareMatrix(int n) : Matrix(n,n) {this->n=n;}

    int getN() const{
        return n;
    }

    SquareMatrix& operator=(const Matrix &M) {
        this->rows=M.getRows();
        this->columns=M.getColumns();
        this->n=M.getRows();
        m = new double*[rows];
        for (int i=0;i<rows;i++) {
            m[i] = new double[columns];
        }
        for (int i=0;i<rows;i++) {
            for (int j=0;j<columns;j++) {
                m[i][j]=M.get(i,j);
            }
        }
        return *this;
    }

    friend ostream & operator << (ostream &out, const SquareMatrix &M){
        for (int i=0;i<M.n;i++) {
            for (int j=0;j<M.n;j++) {
                out<<M.m[i][j]<<' ';
            }
            out<<endl;
        }
        return out;
    }

    friend istream & operator >> (istream &in, SquareMatrix &M){
        in>>M.n;
        M.rows=M.n;
        M.columns=M.n;
        M.m = new double*[M.n];
        for (int i=0;i<M.n;i++) {
            M.m[i] = new double[M.n];
        }
        for (int i=0;i<M.n;i++) {
            for (int j=0;j<M.n;j++) {
                in>>M.m[i][j];
            }
        }
        return in;
    }
};

class IdentityMatrix : public SquareMatrix {

public:
    IdentityMatrix() : SquareMatrix() {}

    IdentityMatrix(int n) : SquareMatrix(n) {
        for (int i=0;i<n;i++) m[i][i]=1;
    }

};

class EliminationMatrix : public IdentityMatrix {
private:
    int i,j;
    Matrix x;

public:
    EliminationMatrix() : IdentityMatrix() {}

    EliminationMatrix(int i,int j,Matrix x) : IdentityMatrix(x.getRows()) {
        this->i=i;
        this->j=j;
        this->x=x;
        m[i][j]=-x.get(i,j)/x.get(j,j);
    }
};

class PermutationMatrix : public IdentityMatrix {

private:
    int i,j;

public:

    PermutationMatrix() : IdentityMatrix() {}

    PermutationMatrix(int i,int j,int n) : IdentityMatrix(n) {
        this->i=i;
        this->j=j;
        swap(m[i][j],m[i][i]);
        swap(m[j][i],m[j][j]);
    }
};

int main()
{
    cout<<fixed<<setprecision(2);
    SquareMatrix A;
    cin>>A;
    ColumnVector V;
    cin>>V;
    int n=A.getN();
    int x=1,y=0;
    int step=1;
    cout<<"step #0:\n"<<A<<V;
    for (int i=0;i<((n)*(n-1))/2;i++) {
        double piv=A.get(y,y);
        int s=-1;
        for (int j=x;j<n;j++) {
            if (abs(A.get(j,y))>abs(piv)) {
                piv=A.get(j,y);
                s=j;
            }
        }
        if (s!=-1) {
            cout<<"step #"<<step<<": permutation\n";
            PermutationMatrix P(y,s,n);
            A=P*A;
            V=P*V;
            cout<<A<<V;
            step++;
        }
        EliminationMatrix E(x,y,A);
        bool skip=(A.get(x,y)==0);
        x++;
        if (x==n) {y++; x=y+1;}
        if (skip) continue;
        cout<<"step #"<<step<<": elimination\n";
        step++;
        A=E*A;
        V=E*V;
        cout<<A<<V;
    }
    y=n-1;
    x=y-1;
    for (int i=0;i<((n)*(n-1))/2;i++) {
        EliminationMatrix E(x,y,A);
        bool skip=(A.get(x,y)==0);
        x--;
        if (x==-1) {y--; x=y-1;}
        if (skip) continue;
        cout<<"step #"<<step<<": elimination\n";
        step++;
        A=E*A;
        V=E*V;
        cout<<A<<V;
    }
    cout<<"Diagonal normalization:\n";
    for (int i=0;i<n;i++) {
        double d=A.get(i,i);
        for (int j=0;j<A.getColumns();j++) {
            if (abs(A.get(i,j))>0.001 && d) A.set(i,j,A.get(i,j)/d);
        }
        if (abs(V.get(i))>0.001 && d) V.set(i,V.get(i)/d);
    }
    cout<<A<<V;
    cout<<"result:\n"<<V;
}


