      program perceptron
            real:: OI(4,3), Y(4,1), W0(3,4), W1(4,1), O1(4,4), O2(4,1)
            real:: DELTA1(4,4), DELTA2(4,1), input0(1,3), input1(1,4)
            real:: ALPHA, start, finish ,ERROR
            !args:
            !O0: training set
            !Y: target val
            !W0: 0th layer weight
            !W1: 1st layer weight
            !O1: first layer val
            !O2: 2nd layer val
            !DELTA1: first layer adjustments
            !DELTA2: 2nd layer adjustments

            open(unit=1,file='XOR_sigmoid.out1',status='unknown')
            write(1,*) 'EPOCH       ERROR'
            
            !FORTRAN USED COLUMN-MAJOR ORDER
            !initializing data sets
            OI = reshape((/0,0,1,1,0,1,0,1,1,1,1,1/), (/4, 3/))
            !Matrix reads:    0 0 1
            !                 0 1 1
            !                 1 0 1
            !                 1 1 1

            Y = reshape((/0,1,1,0/),(/4,1/))

            !initialize random weights for W0 and W1
            do i=1,3
            do j=1,4
                  W0(i,j) = 2.*rand()-1
            end do
            end do

            do i=1,4
                  W1(i,1) = 2.*rand()-1
            end do

            ALPHA = 11

            call cpu_time(start)

            !training
            do i=1,100000
                  O1 = 1./(1.+exp(-matmul(OI,W0)))
                  O2 = 1./(1.+exp(-matmul(O1,W1)))

                  DELTA2 = (O2-Y)*(O2*(1.-O2))
                  DELTA1 = matmul(DELTA2, transpose(W1))*(O1*(1.-O1))

                  W1 = W1 - ALPHA*(matmul(transpose(O1),DELTA2))
                  W0 = W0 - ALPHA*(matmul(transpose(OI),DELTA1))

                  ERROR = sum(0.5*(Y-O2)**2)
                  write(1,*)i,',', ERROR
            end do

            call cpu_time(finish)

            print*, 'Training completed in ', finish-start
            print*, 'Initial learning rate:', ALPHA
            print*, 'Weights:', matmul(W0,W1)
30          print*, 'Enter trial pattern:'
            read(*,*) a, b, c
            input0 = reshape((/a,b,c/),(/1,3/))
            input1 = 1./(1.+exp(-matmul(input0,W0)))
            print*, 1./(1.+exp(-matmul(input1,W1)))
            goto 30

            !write(*,*) O2

            

            
            

      end program perceptron