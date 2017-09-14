# Exploding volleyball

This is a simple 2D game that I wrote to play with Elm. Specifically, I wanted to build my intuition about how to organize Elm applications as they grow. I didn't focus on fun much at all, so it probably only has a few minutes worth of entertainment value.

Try it out [here](https://s3-us-west-1.amazonaws.com/hi-mom-im-on-the-internet/volleyball.html)

## Cross-browser compatibility
### Issues
- Text vertical alignment is broken in all browsers except Chrome.
- Keyboard input on mobile devices will only work with a hardware keyboard
- Text may be too small to read on smartphone screens
- Performance may be poor on mobile devices, especially in landscape orientation

Everything else should be fine in reasonably modern mobile and desktop browsers.

Compatibility was not a priority to me during development, so I'm pretty happy
with this. The vertical alignment issue could be fixed by using the `dy`
attribute of SVG `text` nodes.

## Gameplay
 - Two players bounce a bomb back and forth over a net in the middle of the screen
 - The bomb explodes when it touches the ground or the time runs out
 - Any player that survives the explosion gets a point
 - A new round starts after each explosion
 - Either player can be human or AI controlled
