library(C50)
library(data.table)

posdata <- data.table(read.csv("~/summarization/sentence/pos_7", header=F))
print(posdata)

train.size <- floor(nrow(posdata) * 0.9)
print(train.size)

train.pos <- posdata[1:train.size]
test.pos <- posdata[(train.size+1):nrow(posdata)]
punctree <- C5.0(V8 ~ ., data=train.pos)

print(punctree)

punctreePred <- predict(punctree, test.pos)
punctreeProbs <- data.table(test.pos, predict(punctree, test.pos, type ="prob"))

save(punctree, file="punctree")
save(punctreePred, file="punctreePred")
save(punctreeProbs, file="punctreeProbs")



