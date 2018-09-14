#Project 4

## Simulate an M/M/1 queue assuming customers come according to a poisson process 
## with rate 0.5 person per minute and the service time follows an exponential distribution 
## with mean 3 minutes.  Estimate the mean waiting time for the system.



sim.store=function(arrival.lambda=1/120,service.lambda=1/180,time.close=30000,seed=31415926)
  
{
  
  arrival.time=rexp(1,rate=arrival.lambda)
  
  if(arrival.time<time.close)
    
  {
    
    start.time=arrival.time
    
    waiting.time=0
    
    service.time=rexp(1,rate=service.lambda)
    
    end.time=start.time+service.time
    
    customers=data.frame(customerNo=1,arrival.time,waiting.time,start.time,service.time,end.time)
    
    customer.count=1
    
    wait=0
    
    while(customer.count<1000)
      
    {
      
      arrival.time=arrival.time+rexp(1,rate=arrival.lambda)
      
      if(arrival.time>time.close){break}
      
      start.time=max(arrival.time,end.time)
      
      waiting.time=start.time-arrival.time
      
      wait=wait+waiting.time
      
      service.time=rexp(1,rate=service.lambda)
      
      end.time=start.time+service.time
      
      customer.count=customer.count+1
      
      customers=rbind(customers,c(customerNo=customer.count,arrival.time,waiting.time,start.time,service.time,end.time))
      
    }
    
  }
  
  print(customers)
  
  print("Average waiting time is: ")
  
  print(wait/customer.count)
  
}
sim.store()

  

  
  
  
  
  
  
  
  
 