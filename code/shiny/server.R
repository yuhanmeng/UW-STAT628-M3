library(shiny)
library(Rmisc)
library(ggplot2)
library(scales)
library(leaflet)


data = read.csv("Finalbusiness.csv", header = TRUE,stringsAsFactors = FALSE)
whdata = read.csv("workinghoursAfterLDA.csv", header = TRUE,stringsAsFactors = FALSE)
topic_scores = read.csv("topic_scores.csv", header = TRUE,stringsAsFactors = FALSE)
attrsug = read.csv("suggeations_on_attirbute.csv", header = TRUE,stringsAsFactors = FALSE)
attrib = read.csv("attributes_cleaning.csv", header = TRUE,stringsAsFactors = FALSE)


server = shinyServer(function(input, output) {
  
  businessinfo <- reactive({
    if (is.na(input$B_id) == FALSE){
      businessinfo = data[data$X == as.numeric(input$B_id),]
    }else if(input$B_name != ""){
      businessinfo = data[data$name == input$B_name,][1,]
    }else{
      businessinfo = NA
    }
    })
  
  topictable=data.frame(id=1:5,
                        topic=c("Brunch", "Bar", "Dessert","Fast food","Foreign flavor"))
  
  topicid <- reactive({
    if(length(errmessage())==0){
      if (is.na(input$B_id) == FALSE){
        topicid=as.numeric(businessinfo()[,16])
      }else if(input$B_name != ""){
        topicid=as.numeric(businessinfo()[,16])
      }else{
        topicid = as.numeric(topictable[topictable$topic == input$topic,1])
      }
    }else{
      topicid=0
    }
  })
  
  
  #Tab 1 Start
  output$title1 = renderText({
    if(length(errmessage())==0){
      paste("<font color=\"#7816F5\"><b>",topictable[topictable$id == topicid(),2],"</b></font>")
    }
  })
  output$top101title = renderText({
    if(length(errmessage())==0){
      paste("<font color=\"#1C4FE1\"><b>","Top 10 Food Quality","</b></font>")
    }
  })
  output$top101 = renderTable({
    if(length(errmessage())==0){
      df=topic_scores[topic_scores$X == topicid(),c(2,1,3,4)]
      df=df[order(-df$food_scores),][1:10,]
      for (i in 1:10){
        df[i,2]=data[data$business_id == df[i,1],2]
        df[i,1]=data[data$business_id == df[i,1],11]
      }
      colnames(df)[2]="business ID"
      colnames(df)[4]="food scores"
      df
    }
  })
  output$top102title = renderText({
    if(length(errmessage())==0){
      paste("<font color=\"#1C4FE1\"><b>","Top 10 Atmosphere Quality","</b></font>")
    }
  })
  output$top102 = renderTable({
    if(length(errmessage())==0){
      df=topic_scores[topic_scores$X == topicid(),c(2,1,3,5)]
      df=df[order(-df$overrate_scores),][1:10,]
      for (i in 1:10){
        df[i,2]=data[data$business_id == df[i,1],2]
        df[i,1]=data[data$business_id == df[i,1],11]
      }
      colnames(df)[2]="business ID"
      colnames(df)[4]="atmosphere scores"
      df
    }
  })
  output$top103title = renderText({
    if(length(errmessage())==0){
      paste("<font color=\"#1C4FE1\"><b>","Top 10 Price Quality","</b></font>")
    }
  })
  output$top103 = renderTable({
    if(length(errmessage())==0){
      df=topic_scores[topic_scores$X == topicid(),c(2,1,3,6)]
      df=df[order(-df$price_scores),][1:10,]
      for (i in 1:10){
        df[i,2]=data[data$business_id == df[i,1],2]
        df[i,1]=data[data$business_id == df[i,1],11]
      }
      colnames(df)[2]="business ID"
      colnames(df)[4]="price scores"
      df
    }
  })
  output$top104title = renderText({
    if(length(errmessage())==0){
      paste("<font color=\"#1C4FE1\"><b>","Top 10 Service Quality","</b></font>")
    }
  })
  output$top104 = renderTable({
    if(length(errmessage())==0){
      df=topic_scores[topic_scores$X == topicid(),c(2,1,3,7)]
      df=df[order(-df$service_scores),][1:10,]
      for (i in 1:10){
        df[i,2]=data[data$business_id == df[i,1],2]
        df[i,1]=data[data$business_id == df[i,1],11]
      }
      colnames(df)[2]="business ID"
      colnames(df)[3]="service scores"
      df
    }
  })
  #Tab 1 End
  #Tab 2 Start
  output$info1 = renderTable({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        df=businessinfo()[,c(11,2,14,16)]
        df[,3]=as.character(df[,3])
        df[,4]=topictable[topictable$id == as.numeric(df[,4]),2]
        colnames(df)[2]="business ID"
        colnames(df)[4]="topic"
        df
      }
    }
  })
  output$info2 = renderTable({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        df=businessinfo()[,c(8,3,6,15,12)]
        colnames(df)[1]="is open"
        colnames(df)[5]="postal code"
        df[,5]=as.character(df[,5])
        if (df[1,1]==1){df[1,1]="Yes"} else {df[1,1]="No"}
        df
      }
    }
  })
  output$info3 = renderTable({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        index=which(whdata$business==businessinfo()$business_id)
        if(length(index)>0){
          df=whdata[index,5:11]
          colnames(df)=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
          for (i in 1:7){ if(is.na(df[1,i])) {df[1,i]="NOT Work"}}
          df
        }
      }
    }
  })
  output$plot<-renderPlot({
    if(length(errmessage())==0){
      df=topic_scores[topic_scores$X == topicid(),c(2,3,4,5,6,7)]
      p1=ggplot(df, aes(x=df$food_scores)) + 
        geom_histogram(aes(y=..density..),      
                       binwidth=.01,
                       colour="black", fill="white") +
        labs(x = " food scores") +
        theme(axis.title.y=element_text(colour="darkred", size=20),
              axis.title.x=element_text(colour="darkred", size=20),
              axis.text.y = element_text(colour="darkred", size=18),
              axis.text.x = element_text(colour="darkred", size=18)) +
        geom_density(alpha=.2, fill="#CA2C2C")
      p2=ggplot(df, aes(x=df$overrate_scores)) + 
        geom_histogram(aes(y=..density..),      
                       binwidth=.01,
                       colour="black", fill="white") +
        labs(x = " atmosphere scores") +
        theme(axis.title.y=element_text(colour="blue", size=20),
              axis.title.x=element_text(colour="blue", size=20),
              axis.text.y = element_text(colour="blue", size=18),
              axis.text.x = element_text(colour="blue", size=18)) +
        geom_density(alpha=.2, fill="#C0CA2C")
      p3=ggplot(df, aes(x=df$price_scores)) + 
        geom_histogram(aes(y=..density..),      
                       binwidth=.01,
                       colour="black", fill="white") +
        labs(x = " price scores") +
        theme(axis.title.y=element_text(colour="green", size=20),
              axis.title.x=element_text(colour="green", size=20),
              axis.text.y = element_text(colour="green", size=18),
              axis.text.x = element_text(colour="green", size=18)) +
        geom_density(alpha=.2, fill="#2CCA5E")
      p4=ggplot(df, aes(x=df$service_scores)) + 
        geom_histogram(aes(y=..density..),      
                       binwidth=.01,
                       colour="black", fill="white") +
        labs(x = " service scores") +
        theme(axis.title.y=element_text(colour="purple", size=20),
              axis.title.x=element_text(colour="purple", size=20),
              axis.text.y = element_text(colour="purple", size=18),
              axis.text.x = element_text(colour="purple", size=18)) +
        geom_density(alpha=.2, fill="#2CB9CA")
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        scores=topic_scores[which(topic_scores$names==businessinfo()$business_id),4:7]
        p1=p1+geom_vline(aes(xintercept=scores[1,1]), colour="#FE0000")+
          annotate("text", x=scores[1,1], y=-3,label=as.character(round(scores[1,1],2)),colour="#FE0000",size=6)
        p2=p2+geom_vline(aes(xintercept=scores[1,2]), colour="#FE0000")+
          annotate("text", x=scores[1,2], y=-3,label=as.character(round(scores[1,2],2)),colour="#FE0000",size=6)
        p3=p3+geom_vline(aes(xintercept=scores[1,3]), colour="#FE0000")+
          annotate("text", x=scores[1,3], y=-3,label=as.character(round(scores[1,3],2)),colour="#FE0000",size=6)
        p4=p4+geom_vline(aes(xintercept=scores[1,4]), colour="#FE0000")+
          annotate("text", x=scores[1,4], y=-3,label=as.character(round(scores[1,4],2)),colour="#FE0000",size=6)
      }
      multiplot(p1, p2, p3, p4, cols=2)
    }
  })
  #Tab 2 End
  #Tab 3 Start
  output$title3_1=renderText({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        paste("<font color=\"#FE0000\"><b>","Suggestions about Working time","</b></font>")
      }
    }
  })
  output$title3_2=renderText({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        paste("<font color=\"#FE0000\"><b>","Suggestions based on Attributes","</b></font>")
      }
    }
  })
  output$title3_3=renderText({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        paste("<font color=\"#FE0000\"><b>","Suggestions based on review","</b></font>")
      }
    }
  })
  
  workingtime=data.frame(id=1:5,
                         week=c("4-7","4-7","3-6","5-8","6-9"),
                         weekend=c("4-7","5-8","6-9","5-8","5-8"))
  workingslot=data.frame(id=1:5,
                        week=c("5:00-6:00 and 11:00-15:00","15:00-16:00 and 17:00-20:00","8:00-9:00 and 12:00-16:00 and 17:00-18:00",
                               "8:00-10:00 and 17:00-19:00","9:00-14:00 and 17:00-20:00"),
                        weekend=c("11:00-14:00","15:00-16:00 and 17:00-20:00","8:00-10:00 and 12:00-15:00 and 19:00-20:00",
                                  "17:00-19:00","9:00-10:00 and 16:00-20:00 and 23:00-24:00"))
  
  output$suggestion1 = renderText({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        paste("We recommend you work<br/>",
              "<font color=\"#1C4FE1\"><b>",workingtime[which(workingtime$id==topicid()),2],"</b></font>",
              "hours per day in week <br/>",
              "<font color=\"#1C4FE1\"><b>",workingtime[which(workingtime$id==topicid()),3],"</b></font>",
              "hours per day in weekend","<br/><br/>",
              
              
              "Here are some suggestions of working slots:","<br/>",
              "<font color=\"#1C4FE1\"><b>",workingslot[which(workingslot$id==topicid()),2],"</b></font>",
              "in week","<br/>",
              "<font color=\"#1C4FE1\"><b>",workingslot[which(workingslot$id==topicid()),3],"</b></font>",
              "in weekend.")
      }
    }
  })
  
  output$suggestion2 = renderTable({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        if(topicid()==5){
          df=attrsug[which(attrsug[,1]==topicid()),3]
          df
        }
        else{
          df=attrsug[which(attrsug[,1]==topicid()),]
          indx=c()
          for (i in 1:nrow(df)){
            attribthis=attrib[which(attrib$business_id==businessinfo()$business_id),]
            if(toupper(attribthis[1,which(colnames(attribthis)==df[i,2])])!=toupper(df[i,4])){
              indx=c(indx,i)
            }
          }
          if(length(indx)>0){
            df=df[indx,2:3]
            df
          }else{
            df=data.frame(a=("Well done, keep it!"))
            df
          }
        }
      }
    }
  },colnames = FALSE)
  
  suggest=function(x){
    if(x<0.2){
      s="Perfect!"
    }else if (x<0.4){
      s="Good job."
    }else if (x<0.9){
      s="Need to be improved."
    }else{
      s="It is a critical weakness for you!"
    }
  }
  
  output$suggestion3 = renderTable({
    if(length(errmessage())==0){
      if(is.na(input$B_id) == FALSE | input$B_name != ""){
        scores=topic_scores[which(topic_scores$names==businessinfo()$business_id),4:7]
        scoresdist=topic_scores[topic_scores$X == topicid(),c(2,3,4,5,6,7)]
        a=sum(scoresdist[,3]>=scores[1,1])/nrow(scoresdist)
        b=sum(scoresdist[,4]>=scores[1,2])/nrow(scoresdist)
        c=sum(scoresdist[,5]>=scores[1,3])/nrow(scoresdist)
        d=sum(scoresdist[,6]>=scores[1,4])/nrow(scoresdist)
        df=data.frame(perc=c(paste("Your food performence is at top",percent(a),
                                   "in",topictable[topictable$id == topicid(),2],"topic."),
                             paste("Your atmosphere performence is at top",percent(b),
                                   "in",topictable[topictable$id == topicid(),2],"topic."),
                             paste("Your price performence is at top",percent(c), 
                                   "in",topictable[topictable$id == topicid(),2] ,"topic."),
                             paste("Your service performence is at top",percent(d),
                                   "in",topictable[topictable$id == topicid(),2],"topic.")),
                        suggestion=c(suggest(a),suggest(b),suggest(c),suggest(d)))
        df
      }
    }
  },colnames = FALSE)
  #tab3 end
  
  output$map = renderLeaflet({
    if(is.na(input$B_id) == FALSE | input$B_name != ""){
      center_lng = businessinfo()$longitude
      center_lat = businessinfo()$latitude
      tag=paste(sep = "<br/>",businessinfo()$name,
                paste("Business ID:", as.character(businessinfo()$X)),
                paste("Star:", as.character(businessinfo()$stars)))
      lng = data$longitude[which(data$longitude!=center_lng)]
      lat = data$latitude[which(data$latitude!=center_lat)]
      tags=paste(sep = "<br/>",data[which(data$latitude!=center_lat),]$name,
                 paste("Business ID:", as.character(data[which(data$latitude!=center_lat),]$X)),
                 paste("Star:", as.character(data[which(data$latitude!=center_lat),]$stars)))
      icons <- awesomeIcons(markerColor = "red")
      leaflet() %>% setView(lng = center_lng, lat = center_lat, zoom = 17) %>% addTiles() %>% 
        addAwesomeMarkers(lng=center_lng, lat=center_lat, popup=tag,icon=icons) %>% 
        addMarkers(lng=lng, lat=lat, popup=tags)
    }else{
      center_lng = -80.24601
      center_lat = 40.49618
      lng = data$longitude
      lat = data$latitude
      tags=paste(sep = "<br/>",data$name,
                 paste("Business ID:", as.character(data$X)),
                 paste("Star:", as.character(data$stars)))
      leaflet() %>% setView(lng = center_lng, lat = center_lat, zoom = 17) %>% addTiles() %>% 
        addMarkers(lng=lng, lat=lat, popup=tags)
    }
  })

    errmessage<-reactive({
      errm=c()
      if(is.na(input$B_id) == TRUE & input$B_name == "" & input$topic == ""){
        errm=paste(errm,"Please enter Business ID or Name or select one topic.<br/>")
      }
      
      if(is.na(input$B_id) == FALSE){
        if(as.numeric(input$B_id)>4523 | as.numeric(input$B_id)<0){
          errm=paste(errm,"Sorry! The business ID is invalid!<br/>")
        }else if (input$B_name != "" & data[data$X == as.numeric(input$B_id),]$name!=input$B_name ){
          errm=paste(errm,"Sorry! The business ID and business name is not consistent!<br/>")
        }
      } 
      
      if(input$B_name != "" & nrow(data[data$name == input$B_name,])==0){
        errm=paste(errm,"Sorry! The business name does not exist!<br/>")
      }
      
      paste(errm)
    })
    output$err1<-renderText({
      if(length(errmessage())>0){
        paste("<font color=\"#FF0000\"><b>",errmessage(),"</b></font>")
      }
    })
    output$err2<-renderText({
      if(length(errmessage())>0){
        paste("<font color=\"#FF0000\"><b>",errmessage(),"</b></font>")
      }
    })
    output$err3<-renderText({
      if(length(errmessage())>0){
        paste("<font color=\"#FF0000\"><b>",errmessage(),"</b></font>")
      }else if (is.na(input$B_id) == TRUE & input$B_name == ""){
        paste("<font color=\"#FF0000\"><b>","Please enter Business ID or Name.","</b></font>")
      }
    })

})
