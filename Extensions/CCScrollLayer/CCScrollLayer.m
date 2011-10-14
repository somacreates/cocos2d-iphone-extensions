//
// CCScrollLayer.m
//
// Copyright 2010 DK101
// http://dk101.net/2010/11/30/implementing-page-scrolling-in-cocos2d/
//
// Copyright 2010 Giv Parvaneh.
// http://www.givp.org/blog/2010/12/30/scrolling-menus-in-cocos2d/
//
// Copyright 2011 Stepan Generalov
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#ifndef __MAC_OS_X_VERSION_MAX_ALLOWED

#import "CCScrollLayer.h"
#import "CCGL.h"
#import "SCUtility.h"

enum
{
    kCCScrollLayerStateIdle,
    kCCScrollLayerStateSliding,
};

@interface CCTouchDispatcher (targetedHandlersGetter)

- (NSMutableArray *) targetedHandlers;

@end

@implementation CCTouchDispatcher (targetedHandlersGetter)

- (NSMutableArray *) targetedHandlers
{
    return targetedHandlers;
}

@end

@implementation CCScrollLayer

@synthesize minimumTouchLengthToSlide = minimumTouchLengthToSlide_;
@synthesize minimumTouchLengthToChangePage = minimumTouchLengthToChangePage_;
@synthesize totalScreens = totalScreens_;
@synthesize currentScreen = currentScreen_;
@synthesize showPagesIndicator = showPagesIndicator_;
@synthesize isHorizontal = isHorizontal_;
@synthesize snapToPage = snapToPage_;
@synthesize delegate;

+(id) nodeWithLayers:(NSArray *)layers widthOffset: (int) offset  touchPriority:(int)touchPriority
{
    return [[[self alloc] initWithLayers: layers widthOffset:offset touchPriority:touchPriority] autorelease];
}

-(id) initWithLayers:(NSArray *)layers widthOffset: (int) offset touchPriority:(int)touchPriority
{
    if ( (self = [super init]) )
    {
        NSAssert([layers count], @"CCScrollLayer#initWithLayers:widthOffset: you must provide at least one layer!");
        
        // Enable touches.
        self.isTouchEnabled = YES;
        
        // Set default minimum touch length to scroll.
        self.minimumTouchLengthToSlide = 30.0f;
        self.minimumTouchLengthToChangePage = 100.0f;
        
        // Show indicator by default.
        self.showPagesIndicator = YES;
        
        // Set up the starting variables
        currentScreen_ = 1;
        
        // offset added to show preview of next/previous screens
        scrollDistance_ = self.contentSize.width - offset;
        
        // Horizontal
        isHorizontal_ = YES;
        
        // snapToPage
        snapToPage_ = YES;
        
        touchPriority_ = touchPriority;
        
        // Loop through the array and add the screens
        int i = 0;
        for (CCLayer *l in layers)
        {
            l.anchorPoint = ccp(0,0);
            l.position = ccp((i*scrollDistance_),0);
            [self addChild:l];
            i++;
        }
        
        // Setup a count of the available screens
        totalScreens_ = [layers count];
        
    }
    return self;
}

+(id) nodeWithLayers:(NSArray *)layers heightOffset: (int) offset  touchPriority:(int)touchPriority
{
    return [[[self alloc] initWithLayers: layers heightOffset:offset touchPriority:touchPriority] autorelease];
}

-(id) initWithLayers:(NSArray *)layers heightOffset: (int) offset  touchPriority:(int)touchPriority
{
    if ( (self = [super init]) )
    {
        NSAssert([layers count], @"CCScrollLayer#initWithLayers:heightOffset: you must provide at least one layer!");
        
        // Enable touches.
        self.isTouchEnabled = YES;
        
        // Set default minimum touch length to scroll.
        self.minimumTouchLengthToSlide = 30.0f;
        self.minimumTouchLengthToChangePage = 100.0f;
        
        // Show indicator by default.
        self.showPagesIndicator = YES;
        
        // Set up the starting variables
        currentScreen_ = 1;
        
        // offset added to show preview of next/previous screens
        scrollDistance_ = offset;
        
        // Vertical
        isHorizontal_ = NO;
        
        // snapToPage
        snapToPage_ = YES;
        
        touchPriority_ = touchPriority;
        
        // Loop through the array and add the screens
        int i = 0;
        for (CCLayer *l in layers)
        {
            l.anchorPoint = ccp(0,0);
            l.position = ccp(l.position.x,-(i*scrollDistance_));
            [self addChild:l];
            i++;
        }
        
        // Setup a count of the available screens
        totalScreens_ = [layers count];
        
    }
    return self;
}

-(CGRect) layerRect {
    CGRect r = self.boundingBox;
	r = CGRectMake(self.position.x - r.size.width , 
                   0.0f, 
                   r.size.width * 2.0f, 
                   r.size.height);
    return r;
}


#pragma mark CCLayer Methods ReImpl

// Register with more priority than CCMenu's but don't swallow touches
-(void) registerWithTouchDispatcher
{
    [[CCTouchDispatcher sharedDispatcher] addTargetedDelegate:self priority:touchPriority_ swallowsTouches:NO];
}

- (void) visit
{
    [super visit];//< Will draw after glPopScene.
    
    if (self.showPagesIndicator)
    {
        // Prepare Points Array
        CGFloat n = (CGFloat)totalScreens_; //< Total points count in CGFloat.
        CGFloat d = 16.0f; //< Distance between points.
        CGPoint points[totalScreens_];
        
        if (isHorizontal_ == YES) {
            CGFloat pY = ceilf ( self.contentSize.height / 8.0f ); //< Points y-coord in parent coord sys.
            for (int i=0; i < totalScreens_; ++i)
            {
                CGFloat pX = 0.5f * self.contentSize.width + d * ( (CGFloat)i - 0.5f*(n-1.0f) );
                points[i] = ccp (pX, pY);
            }
        } else {
            CGFloat pX = self.contentSize.width - ceilf ( self.contentSize.width / 8.0f ); //< Points x-coord in parent coord sys.
            for (int i=0; i < totalScreens_; ++i)
            {
                CGFloat pY = 0.5f * self.contentSize.height - d * ( (CGFloat)i - 0.5f*(n-1.0f) );
                points[i] = ccp (pX, pY);
            }
        }
        
        // Set GL Values
        glEnable(GL_POINT_SMOOTH);
        GLboolean blendWasEnabled = glIsEnabled( GL_BLEND );
        glEnable(GL_BLEND);
        glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
        glPointSize( 6.0 * CC_CONTENT_SCALE_FACTOR() );
        
        // Draw Gray Points
        glColor4ub(0x96,0x96,0x96,0xFF);
        ccDrawPoints( points, totalScreens_ );
        
        // Draw White Point for Selected Page
        glColor4ub(0xFF,0xFF,0xFF,0xFF);
        ccDrawPoint(points[currentScreen_ - 1]);
        
        // Restore GL Values
        glPointSize(1.0f);
        glDisable(GL_POINT_SMOOTH);
        if (! blendWasEnabled)
            glDisable(GL_BLEND);
    }
}

#pragma mark Pages Control

-(void) moveToPage:(int)page
{
    int changeX = self.position.x;
    int changeY = self.position.y;
    
    if (isHorizontal_ == YES) {
        changeX = -((page-1)*scrollDistance_);
    } else {
        changeY = ((page-1)*scrollDistance_);
    }
    
    id changePage = [CCMoveTo actionWithDuration:0.3 position:ccp(changeX,changeY)];
    [self runAction:changePage];
    currentScreen_ = page;
}

#pragma mark Hackish Stuff

- (void) claimTouch: (UITouch *) aTouch
{
    // Enumerate through all targeted handlers.
    for ( CCTargetedTouchHandler *handler in [[CCTouchDispatcher sharedDispatcher] targetedHandlers] )
    {
        // Only our handler should claim the touch.
        if (handler.delegate == self)
        {
            if (![handler.claimedTouches containsObject: aTouch])
            {
                [handler.claimedTouches addObject: aTouch];
            }
            else
            {
                CCLOGERROR(@"CCScrollLayer#claimTouch: %@ is already claimed!", aTouch);
            }
            return;
        }
    }
}

- (void) cancelAndStoleTouch:(UITouch *)touch withEvent:(UIEvent *)event
{
    // Throw Cancel message for everybody in TouchDispatcher.
    [[CCTouchDispatcher sharedDispatcher] touchesCancelled: [NSSet setWithObject: touch] withEvent:event];
    
    //< after doing this touch is already removed from all targeted handlers
    
    // Squirrel away the touch
    [self claimTouch: touch];
}

-(void) insignificantMovementCheck {
    CGFloat distanceToCheck = ccpDistance(startTouch_, lastTouch_);
    
    if (distanceToCheck < (iPhonep() ? 20.0f : 45.0f)) {
        cancelNextTouch_ = YES;
        
        if ([self.delegate respondsToSelector:@selector(scrollLayerDidHaveInsignificantMovement:)]) {
            [self.delegate scrollLayerDidHaveInsignificantMovement:self];
        }
    }
}

#pragma mark Touches

-(BOOL) ccTouchBegan:(UITouch *)touch withEvent:(UIEvent *)event
{
    CGPoint touchPoint = [touch locationInView:[touch view]];
    touchPoint = [[CCDirector sharedDirector] convertToGL:touchPoint];
    
    // If we are not in the bounds of our layer then we early exit...
    if (!CGRectContainsPoint([self layerRect], touchPoint)) {
        return NO;
    }
    
    startSwipe_ = touchPoint;
    
    startTouch_ = touchPoint;
    
    lastTouch_ = touchPoint;
    
    didStartDraggingVertical_ = NO;
    
    state_ = kCCScrollLayerStateIdle;
    
    
    id minDistanceCheck = [CCSequence actions:
                           [CCDelayTime actionWithDuration:0.5f],
                           [CCCallFunc actionWithTarget:self selector:@selector(insignificantMovementCheck)],
                           nil];
    
    [self runAction:minDistanceCheck];
    
    return YES;
}

- (void)ccTouchMoved:(UITouch *)touch withEvent:(UIEvent *)event
{
    if (cancelNextTouch_) {
        cancelNextTouch_ = NO;
        [[CCTouchDispatcher sharedDispatcher] touchesCancelled: [NSSet setWithObject: touch] withEvent:event];
    }
    
    CGPoint touchPoint = [touch locationInView:[touch view]];
    touchPoint = [[CCDirector sharedDirector] convertToGL:touchPoint];
    
    lastTouch_ = touchPoint;
    
    int moveDistance = 0;
    if (isHorizontal_ == YES) {
        moveDistance = touchPoint.x-startSwipe_.x;
    } else {
        moveDistance = touchPoint.y-startSwipe_.y;
    }
    
    
    // Quick hack for our game... 
    // If we have a vertical move we want to claim it. If not we want to
    // let it behave as normal...
    if (!isHorizontal_ && !didStartDraggingVertical_) {
        if (fabsf(touchPoint.x - startSwipe_.x) >= self.minimumTouchLengthToSlide) {
            [[CCTouchDispatcher sharedDispatcher] touchesCancelled: [NSSet setWithObject: touch] withEvent:event];
            
            if ([self.delegate respondsToSelector:@selector(scrollLayer:scrollingStartedHorizontalWithTouch:)]) {
                [self.delegate scrollLayer:self scrollingStartedHorizontalWithTouch:touch];
            }
            return;
        }
    }
    
    
    // If finger is dragged for more distance then minimum - start sliding and cancel pressed buttons.
    // Of course only if we not already in sliding mode
    if ( (state_ != kCCScrollLayerStateSliding)
        && (fabsf(moveDistance) >= self.minimumTouchLengthToSlide) )
    {
        state_ = kCCScrollLayerStateSliding;
        
        didStartDraggingVertical_ = YES;
        
        startSwipe_ = cpvsub(self.position, touchPoint);
        

        [self cancelAndStoleTouch: touch withEvent: event];
        
        if ([self.delegate respondsToSelector:@selector(scrollLayer:scrollingStartedVerticalWithTouch:)]) {
            [self.delegate scrollLayer:self scrollingStartedVerticalWithTouch:touch];
        }
    }
    
    if (state_ == kCCScrollLayerStateSliding) {
        int pointX = self.position.x;
        int pointY = self.position.y;
        
        
        if (isHorizontal_ == YES) {
            pointX = (-(currentScreen_-1)*scrollDistance_)+(touchPoint.x-startSwipe_.x);
        } else {
            pointY = ((currentScreen_-1)*scrollDistance_)+(touchPoint.y+startSwipe_.y);
        }
        self.position = ccp(pointX,pointY);
    }
}

- (void)ccTouchEnded:(UITouch *)touch withEvent:(UIEvent *)event
{
    if (cancelNextTouch_) {
        cancelNextTouch_ = NO;
        [[CCTouchDispatcher sharedDispatcher] touchesCancelled: [NSSet setWithObject: touch] withEvent:event];
    }
    
    if (snapToPage_) {
        CGPoint touchPoint = [touch locationInView:[touch view]];
        touchPoint = [[CCDirector sharedDirector] convertToGL:touchPoint];
        
        lastTouch_ = touchPoint;
        
        int offsetLoc = 0;
        if (isHorizontal_ == YES) {
            offsetLoc = (touchPoint.x - startSwipe_.x);
        } else {
            offsetLoc = -(touchPoint.y - startSwipe_.y);
        }
        
        
        if ( offsetLoc < -self.minimumTouchLengthToChangePage && (currentScreen_+1) <= totalScreens_ )
        {
            [self moveToPage: currentScreen_+1];
        }
        else if ( offsetLoc > self.minimumTouchLengthToChangePage && (currentScreen_-1) > 0 )
        {
            [self moveToPage: currentScreen_-1];
        }
        else
        {
            [self moveToPage:currentScreen_];
        }
    } else {
        // Get the highest and lowest positioned layers
        CCNode *lowest = nil;
        CCNode *highest = nil;
        for (CCNode *node in self.children) {
            if (node.position.y < (lowest ? lowest.position.y : MAXFLOAT)) {
                lowest = node;
            }
            
            if (node.position.y > (highest ? highest.position.y : -MAXFLOAT)) {
                highest = node;
            }
        }
        
        NSAssert(lowest && highest,
                 @"Unable to find the lowest and highest layers");
        
        
        // Now check and see if we are out of bounds...
        
        // If our lowest layer is above 0 + (/ lowerlayerheight 2) then
        // move to lowerlayerheight / 2
        
        if (self.position.y < self.contentSize.height - (highest.contentSize.height / 2.0f)) {
            CGPoint newPosition = ccp(self.position.x, 
                                      self.contentSize.height - (highest.contentSize.height));
            
            [self runAction:[CCMoveTo actionWithDuration:0.3 
                                                position:newPosition]];
        } else if (self.position.y > 0.0f - lowest.position.y) {
            CGPoint newPosition = ccp(self.position.x, 
                                      -lowest.position.y + (lowest.contentSize.height / 2.0f));
            
            [self runAction:[CCMoveTo actionWithDuration:0.3 
                                                position:newPosition]];
        }
        
    }
}


#endif

#pragma mark Mouse
#ifdef __MAC_OS_X_VERSION_MAX_ALLOWED

- (NSInteger) mouseDelegatePriority
{
	return kCCMenuMousePriority - 1;
}

-(BOOL) ccMouseDown:(NSEvent*)event
{
	CGPoint touchPoint = [[CCDirector sharedDirector] convertEventToGL: event];
	
	startSwipe_ = touchPoint.x;
	state_ = kCCScrollLayerStateIdle;
	
	return NO;
}

-(BOOL) ccMouseDragged:(NSEvent*)event
{
	CGPoint touchPoint = [[CCDirector sharedDirector] convertEventToGL:event];
	
	// If mouse is dragged for more distance then minimum - start sliding.
	if ( (state_ != kCCScrollLayerStateSliding) 
		&& (fabsf(touchPoint.x-startSwipe_) >= self.minimumTouchLengthToSlide) )
	{
		state_ = kCCScrollLayerStateSliding;
		
		// Avoid jerk after state change.
		startSwipe_ = touchPoint.x;
		
		if ([self.delegate respondsToSelector:@selector(scrollLayerScrollingStarted:)])
		{
			[self.delegate scrollLayerScrollingStarted: self];
		}
	}
	
	if (state_ == kCCScrollLayerStateSliding)
		self.position = ccp( (- currentScreen_ * (self.contentSize.width - self.pagesWidthOffset)) + (touchPoint.x-startSwipe_),0);	
	
	return NO;
}

- (BOOL)ccMouseUp:(NSEvent *)event
{
	CGPoint touchPoint = [[CCDirector sharedDirector] convertEventToGL:event];
	
	int newX = touchPoint.x;	
	
	if ( (newX - startSwipe_) < -self.minimumTouchLengthToChangePage && (currentScreen_+1) < [layers_ count] )
	{		
		[self moveToPage: [self pageNumberForPosition: self.position] ];		
	}
	else if ( (newX - startSwipe_) > self.minimumTouchLengthToChangePage && currentScreen_ > 0 )
	{		
		[self moveToPage: [self pageNumberForPosition:self.position] ];		
	}
	else
	{		
		[self moveToPage:currentScreen_];		
	}	
	
	return NO;
}

- (BOOL)ccScrollWheel:(NSEvent *)theEvent
{
	CGFloat deltaX = [theEvent deltaX];
	
	CGPoint newPos = ccpAdd( self.position, ccp(deltaX, 0.0f) );
	newPos.x = MIN(newPos.x, [self positionForPageWithNumber: 0].x);
	newPos.x = MAX(newPos.x, [self positionForPageWithNumber: [layers_ count] - 1].x);
	
	self.position = newPos;
	currentScreen_ = [self pageNumberForPosition:self.position];
	
	return NO;
	
}

#endif

@end

