a=matrix(NA,4,4)
s=c(1:16)
x=c(2,4)
s2=c(0,0)
s1=sample(x,2)
s2[c(1,2)]=sample(x,1)
s3=c(s1,s2)
a[sample(s,2)]<-sample(s3,2)
u="good"
k=0

game<-function(inp,a){
  
  change_position_l<-function(x){
    ponu=grep("[1-9]",x)
    ponas=which(is.na(x))
    poss=c(ponu,ponas)
    x<-x[poss]
    return(x)
  }
  change_position_R<-function(x){
    ponu=grep("[1-9]",x)
    ponas=which(is.na(x))
    poss=c(ponas,ponu)
    x<-x[poss]
    return(x)
  }
  
  consecutive<-function(x){
    if ( T %in% (x[-1]-x[-length(x)]==1)){
      return (T)
    }else{
      return (F)
    }
  }
  
  add<-function(x){
    if (sum(is.na(x))==4 & sum(grep("[1-9]",x))==0){
      return(F)
    }else if (!(T %in% duplicated(x))){
      return(F)
    }else if (T %in% duplicated(x)){
      v=x[!(is.na(x))]
      ma=v[duplicated(v)]
      po=grep(ma[1],x)
      if (consecutive(po)==T){
        return(T)
      }else{
        return(F)
      }
    }
  }
  
  addd_R<-function(x){
    v=x[!(is.na(x))]
    uni=length(unique(v[duplicated(v)]))
    ma=v[duplicated(v)]
    if (uni==1 & length(grep(paste0("^",ma[1],"$"),x))==4){
      uni=2
    }
    a=1
    while(a<=uni){
      ma=v[duplicated(v)]
      po=grep(paste0("^",ma[a],"$"),x)
      if (consecutive(po)==T){
        he=vector()
        if (length(grep(paste0("^",ma[1],"$"),x))==4){
          he=head(po,2)
        }else if(length(grep(paste0("^",ma[1],"$"),x))!=4){
          z=po[-1][po[-1]-po[-length(po)]==1]
          zz=po[-length(po)][po[-1]-po[-length(po)]==1]
          he=head(c(z,zz),2)
        }
        ad=sum(x[he])
        g=max(he)
        x[g]<-ad
        s=min(he)
        x[s]<-NA
        a=a+1
      }else if (consecutive(po)==F){
        a=a+1
      }
    }
    return(x)
  }
  addd_l<-function(x){
    v=x[!(is.na(x))]
    uni=length(unique(v[duplicated(v)]))
    ma=v[duplicated(v)]
    if (uni==1 & length(grep(paste0("^",ma[1],"$"),x))==4){
      uni=2
    }
    a=1
    while(a<=uni){
      ma=v[duplicated(v)]
      po=grep(paste0("^",ma[a],"$"),x)
      if (consecutive(po)==T){
        he=vector()
        if (length(grep(paste0("^",ma[1],"$"),x))==4){
          he=head(po,2)
        }else if(sum(po[-1]-po[-length(po)]==1)==2){
          z=po[-1][po[-1]-po[-length(po)]==1]
          zz=po[-length(po)][po[-1]-po[-length(po)]==1]
          he=head(c(zz),2)
        }else if(sum(po[-1]-po[-length(po)]==1)==1){
          z=po[-1][po[-1]-po[-length(po)]==1]
          zz=po[-length(po)][po[-1]-po[-length(po)]==1]
          he=head(c(z,zz),2)
        }
        ad=sum(x[he])
        g=max(he)
        x[g]<-NA
        s=min(he)
        x[s]<-ad
        a=a+1
      }else if (consecutive(po)==F){
        a=a+1
      }
    }
    return(x)
  }
  
  end<-function(a){
    ret1=3
    ret2=3
    ret3=3
    ret4=3
    co=1
    while (co<5){
      x=a[,co]
      v=x[!(is.na(x))]
      ma=v[duplicated(v)]
      po=grep(paste0("^",ma[1],"$"),x)
      if (consecutive(po)==T & co==1){
        ret1=1
        co=co+1
      }else if (consecutive(po)==F & co==1){
        ret1=0
        co=co+1
      }else if (consecutive(po)==T & co==2){
        ret2=1
        co=co+1
      }else if (consecutive(po)==F & co==2){
        ret2=0
        co=co+1
      }else if (consecutive(po)==T & co==3){
        ret3=1
        co=co+1
      }else if (consecutive(po)==F & co==3){
        ret3=0
        co=co+1
      }else if (consecutive(po)==T & co==4){
        ret4=1
        co=co+1
      }else if (consecutive(po)==F & co==4){
        ret4=0
        co=co+1
      }
    }
    re1=3
    re2=3
    re3=3
    re4=3
    ro=1
    while (ro<5){
      x=a[ro,]
      v=x[!(is.na(x))]
      ma=v[duplicated(v)]
      po=grep(paste0("^",ma[1],"$"),x)
      if (consecutive(po)==T & ro==1){
        re1=1
        ro=ro+1
      }else if (consecutive(po)==F & ro==1){
        re1=0
        ro=ro+1
      }else if (consecutive(po)==T & ro==2){
        re2=1
        ro=ro+1
      }else if (consecutive(po)==F & ro==2){
        re2=0
        ro=ro+1
      }else if (consecutive(po)==T & ro==3){
        re3=1
        ro=ro+1
      }else if (consecutive(po)==F & ro==3){
        re3=0
        ro=ro+1
      }else if (consecutive(po)==T & ro==4){
        re4=1
        ro=ro+1
      }else if (consecutive(po)==F & ro==4){
        re4=0
        ro=ro+1
      }
    }
    if(sum(c(ret1,ret2,ret3,ret4))==0 & sum(c(re1,re2,re3,re4))==0){
      return(T)
    }else{
      return(F)
    }
  }
  if (sum(is.na(a))==0 & end(a)==T){
    u<<-"YOU LOST THE GAME :( "
    return(u)
  }else if (2048 %in% a){
    u<<-"YOU WON THE GAME !!!"
    return(u)
  }else if (toupper(inp)=="E"){
    mat=a
    k<<-0
    e=1
    while (e<5){
      a[,e]<-change_position_l(a[,e])
      if (add(a[,e])==F){
        a[,e]<-change_position_l(a[,e])
        e=e+1
      }else if (add(a[,e])==T){
        a[,e]<-addd_l(a[,e])
        a[,e]<-change_position_l(a[,e])
        e=e+1
      }
    }
    if (identical(mat,a)){
      k<<-1
    }
    if (sum(is.na(a))>1 & k==0){
      s=grep(T,is.na(a))
      a[sample(s,1)]<-sample(x,1)
      a<<-a
      return(a)  
    }else if (sum(is.na(a))==1 & k==0){
      s=grep(T,is.na(a))
      a[s]<-sample(x,1)
      a<<-a
      return(a) 
    }
  }else if (toupper(inp)=="D"){
    mat=a
    k<<-0
    d=1
    while (d<5){
      a[,d]<-change_position_R(a[,d])
      if (add(a[,d])==F){
        a[,d]<-change_position_R(a[,d])
        d=d+1
      }else if (add(a[,d])==T){
        a[,d]<-addd_R(a[,d])
        a[,d]<-change_position_R(a[,d])
        d=d+1
      }
    }
    if (identical(mat,a)){
      k<<-1
    }
    if (sum(is.na(a))>1 & k==0){
      s=grep(T,is.na(a))
      a[sample(s,1)]<-sample(x,1)
      a<<-a
      return(a)   
    }else if (sum(is.na(a))==1 & k==0){
      s=grep(T,is.na(a))
      a[s]<-sample(x,1)
      a<<-a
      return(a) 
    }
  }else if (toupper(inp)=="S"){
    mat=a
    k<<-0
    s=1
    while (s<5){
      a[s,]<-change_position_l(a[s,])
      if (add(a[s,])==F){
        a[s,]<-change_position_l(a[s,])
        s=s+1
      }else if (add(a[s,])==T){
        a[s,]<-addd_l(a[s,])
        a[s,]<-change_position_l(a[s,])
        s=s+1
      }
    }
    if (identical(mat,a)){
      k<<-1
    }
    if (sum(is.na(a))>1 & k==0){
      ss=grep(T,is.na(a))
      a[sample(ss,1)]<-sample(x,1)
      a<<-a
      return(a)  
    }else if (sum(is.na(a))==1 & k==0){
      ss=grep(T,is.na(a))
      a[ss]<-sample(x,1)
      a<<-a
      return(a) 
    }
  }else if (toupper(inp)=="F"){
    mat=a
    k<<-0
    f=1
    while (f<5){
      a[f,]<-change_position_R(a[f,])
      if (add(a[f,])==F){
        a[f,]<-change_position_R(a[f,])
        f=f+1
      }else if (add(a[f,])==T){
        a[f,]<-addd_R(a[f,])
        a[f,]<-change_position_R(a[f,])
        f=f+1
      }
    }
    if (identical(mat,a)){
      k<<-1
    }
    if (sum(is.na(a))>1 & k==0){
      s=grep(T,is.na(a))
      a[sample(s,1)]<-sample(x,1)
      a<<-a
      return(a)   
    }else if (sum(is.na(a))==1 & k==0){
      s=grep(T,is.na(a))
      a[s]<-sample(x,1)
      a<<-a
      return(a)   
    }
  }
  
}

library(shiny)

shinyServer(function(input, output) {
  
  observeEvent(input$start, {output$table <- renderTable({
    return(a)
  },bordered=T, colnames = F, width = "100", digits = 0) 
  }) 
  
  observeEvent(input$up, {
    inp="e"
    game(inp,a)
    if(u=="YOU LOST THE GAME :( "){
      output$text <- renderText({
        return(u)
      })
    }else if(u=="YOU WON THE GAME !!!"){
      output$text <- renderText({
        return(u)
      })
    }else if (u=="good") {output$table <- renderTable({
      return(a)
    },bordered=T, colnames = F, width = "100", digits = 0)
    }
  })
  observeEvent(input$down, {
    inp="d"
    game(inp,a)
    if(u=="YOU LOST THE GAME :( "){
      output$text <- renderText({
        return(u)
      })
    }else if(u=="YOU WON THE GAME !!!"){
      output$text <- renderText({
        return(u)
      })
    }else if (u=="good") {output$table <- renderTable({
      return(a)
    },bordered=T, colnames = F, width = "100", digits = 0)
    }
  })
  observeEvent(input$left, {
    inp="s"
    game(inp,a)
    if(u=="YOU LOST THE GAME :( "){
      output$text <- renderText({
        return(u)
      })
    }else if(u=="YOU WON THE GAME !!!"){
      output$text <- renderText({
        return(u)
      })
    }else if (u=="good") {output$table <- renderTable({
      return(a)
    },bordered=T, colnames = F, width = "100", digits = 0)
    }
  })
  observeEvent (input$right, {
    inp="f"
    game(inp,a)
    if(u=="YOU LOST THE GAME :( "){
      output$text <- renderText({
        return(u)
      })
    }else if(u=="YOU WON THE GAME !!!"){
      output$text <- renderText({
        return(u)
      })
    }else if (u=="good") output$table <- renderTable({
      return(a)
    },bordered=T, colnames = F, width = "100", digits = 0)
  }
  )
})


