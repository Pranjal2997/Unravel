# Surface-Crack
Detection of surface cracks is an important task in monitoring the structural health of concrete structures. If cracks develop and continue to propogate, they reduce the effective load bearing surface area and can over time cause failure of the structure. The manual process of crack detection is painstakingly time-consuming and suffers from subjective judgments of inspectors. Manual inspection can also be difficult to perform in case of high rise buildings and bridges. In this hackathon, we have used deep learning to build a simple yet very accurate model for crack detection. 

The data set consists of 300 images of concrete structures with cracks and 300 images each without cracks. The dataset is generated from 458 high-resolution images (4032x3024 pixel). Each image in the data set is a 227 x 227 pixels RGB image. Some sample images with cracks and without cracks are shown below:

With Crack
![image](https://user-images.githubusercontent.com/111147280/213517763-4f3f9ff4-64b2-4c70-907b-82cf1fcb4bc9.png)

Without Crack
![image](https://user-images.githubusercontent.com/111147280/213517874-353bbbc5-519a-423a-8c9c-91495584bbf9.png)

Model Build
For this problem, we have built a Convolution Neural Network (CNN) in Pytorch. Since we have a limited number of images, we will use a pretrained network as a starting point and use image augmentations to further improve accuracy. Image augmentations allow us to do transformations like — vertical and horizontal flip, rotation and brightness changes significantly increasing the sample and helping the model generalize.

RESNET Architecture
ResNet is a well-known deep learning model that was first introduced in a paper by Shaoqing Ren, Kaiming He, Jian Sun, and Xiangyu Zhang. In 2015, a study titled "Deep Residual Learning for Image Recognition" was published. ResNet is one of the most widely used and effective deep learning models to date.

ResNets are made up of what's known as a residual block.
This is built on the concept of "skip-connections" and uses a lot of batch-normalization to let it train hundreds of layers successfully without sacrificing speed over time.

![image](https://user-images.githubusercontent.com/111147280/213521867-265dd51e-12c9-4ef9-89a5-fe9ce716e3e2.png)
The first thing we note in the above diagram is that there is a direct link that skips several of the model's levels. The'skip connection,' as it is known, lies at the core of residual blocks. Because of the skip connection, the output is not the same. Without the skip connection, input 'X is multiplied by the layer's weights, then adding of a bias term.

The Architecture design is inspired on VGG-19 and has a 34-layer plain network to which shortcut and skip connections are added. As seen in the diagram below, these skip connections or residual blocks change the design into a residual network.

Complete Architecture:

![image](https://user-images.githubusercontent.com/111147280/213522031-625c5e9b-c4be-45f3-9999-d17e58cdce7d.png)


The idea was that the deeper layers shouldn't have any more training mistakes than their shallower equivalents. To put this notion into action, skip-connections were created. The creators of this network used a pre-activation variation of the residual block in which gradients can flow through the shortcut link to the earlier layers, minimising the problem of "vanishing gradients."


Also we have tried Inception Net and VGG Model Architecture as well

VGG
VGG is a convolutional neural network design that has been around for a long time. It was based on a study on how to make such networks more dense. Small 3 x 3 filters are used in the network. The network is otherwise defined by its simplicity, with simply pooling layers and a fully linked layer as additional components.

In comparison to AlexNet and ZfNet, VGG was created with 19 layers deep to replicate the relationship between depth and network representational capability.

Small size filters can increase the performance of CNNs, according to ZfNet, a frontline network in the 2013-ILSVRC competition. Based on these observations, VGG replaced the 11x11 and 5x5 filters with a stack of 3x3 filters, demonstrating that the simultaneous placement of small size (3x3) filters may provide the effect of a big size filter (5x5 and 7x7). By lowering the number of parameters, the usage of tiny size filters gives an additional benefit of low computing complexity. These discoveries ushered in a new research trend at CNN, which is to work with lower size filters.


![image](https://user-images.githubusercontent.com/111147280/213522492-2dd2230e-cfcb-4ed6-9321-3ace216eb72a.png)


InceptionNet

The inception Module looks like this:
![image](https://user-images.githubusercontent.com/111147280/213522805-15c87402-0b3d-41a5-bcd2-e099fbd75387.png)


The InceptionNet design is made up of nine inception modules stacked on top of each other, with max-pooling layers between them (to halve the spatial dimensions). It is made up of 22 layers (27 with the pooling layers). After the last inception module, it employs global average pooling.




We have followed following steps for our dataset:
1.Shuffle and Split input data into Train and Val
2.Apply Transformations

Pytorch makes it easy to apply data transformations which can augment training data and help the model generalize. The transformations I chose were random rotation, random horizontal and vertical flip as well as random color jitter. Also, each channel is divided by 255 and then normalized. This helps with the neural network training.

Pretrained Model

We are using a Resnet 50 model pretrained on ImageNet to jump start the model.
As shown below the ResNet50 model consists of 5 stages each with a convolution and Identity block. Each convolution block has 3 convolution layers and each identity block also has 3 convolution layers. The ResNet-50 has over 23 million trainable parameters. We are going to freeze all these weights and 2 more fully connected layers — The first layer has 128 neurons in the output and the second layer has 2 neurons in the output which are the final predictions.



![image](https://user-images.githubusercontent.com/111147280/213518574-666c8f7f-0e2d-4c15-bbc9-3551bf013639.png)


![image](https://user-images.githubusercontent.com/111147280/213519735-26eada9f-b4cc-4109-94f3-d15a73e955af.png)


![image](https://user-images.githubusercontent.com/111147280/213520378-1a685a84-5978-4812-b560-32169879875f.png)



Model Training and Prediction on Real Images
We use transfer learning to then train the model on the training data set while measuring loss and accuracy on the validation set. As shown by the loss and accuracy numbers below, the model trains very quickly. For the last epoch, F1 score is 94.34% and validation accuracy is 94%!. This is the power of transfer learning. Our final model has a validation accuracy of 94% , Precision of 89.29%, Recall of 100% , F1 score is 94.34%.

The model does very well on images that it has not seen before. As shown in the image below, the model is able to detect a very long crack in concrete by processing 100s of patches on the image.


![image](https://user-images.githubusercontent.com/111147280/213521093-c9071ccb-27dc-4613-b0a9-54df85f1ed53.png)

![image](https://user-images.githubusercontent.com/111147280/213521118-cd8bb56b-585c-4e80-8339-dc46684b9ad0.png)


![image](https://user-images.githubusercontent.com/111147280/213521229-90aca957-1f4e-4f99-b1fc-56a6191f7979.png)


![image](https://user-images.githubusercontent.com/111147280/213521290-c814af53-2442-405d-940e-3baab1f59f0e.png)


Conclusion
This file shows how easy it has become to build real world applications using deep learning and open source data.

