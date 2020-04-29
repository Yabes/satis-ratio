import { Elm } from './Main.elm';
import * as d3base from 'd3';
import { sankey, sankeyJustify, sankeyLinkHorizontal } from 'd3-sankey';
import { transition } from 'd3-transition';
import * as serviceWorker from './serviceWorker';

const d3 = Object.assign(d3base, {
  sankey,
  sankeyJustify,
  sankeyLinkHorizontal,
});

const width = window.innerWidth - 100;
const height = 600;

const diagram = d3
  .sankey()
  .nodeId(d => d.id)
  .nodeAlign(d3.sankeyJustify)
  .nodeWidth(15)
  .nodePadding(100)
  .iterations(100)
  .extent([[1, 5], [width - 1, height - 5]]);

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    width,
    height
  }
});

function nodeTitle(d) {
  const input = d.targetLinks.length > 0
    ? '\n\nInput:\n' + d.targetLinks.map(l => l.title).join('\n')
    : '';

  const output = d.sourceLinks.length > 0
    ? '\n\nUsed output:\n' + d.sourceLinks.map(l => l.title).join('\n')
    : '';

  const extra = d.extra.charAt(0) !== '0'
    ? '\n\nExtra output:\n' + d.extra
    : '';

  return `${d.title}${input}${output}${extra}`;
}

const svg = d3.select('#sankey').attr('viewBox', [0, 0, width, height]);
svg.append('g').attr('class', 'nodes');
svg.append('g').attr('class', 'edges');
svg.append('g').attr('class', 'texts');

const t = svg.transition().duration(700);

app.ports.setDiagramData.subscribe(data => {
  const { nodes, links } = diagram(data);

  svg.selectAll("*").interrupt();

  if (links.length === 0 || nodes.length === 0) {
    svg.selectAll('g > *').remove();
    return;
  }

  svg
    .select('g.nodes')
    .selectAll('rect')
    .data(nodes, d => d.production)
    .join(
      enter =>
        enter
          .append('rect')
          .attr('fill', d => d.color)
          .attr('height', d => d.y1 - d.y0)
          .attr('width', d => d.x1 - d.x0)
          .attr('x', d => d.x0)
          .attr('y', d => d.y0)
          .attr('fill-opacity', 0)
          .call(enter_ =>
            enter_
              .transition(t)
              .attr('fill-opacity', 1)
          )
      .append('title')
      .text(nodeTitle)
      ,
      update =>
        update
          .call(update_ =>
            update_
              .transition(t)
              .attr('x', d => d.x0)
              .attr('y', d => d.y0)
              .attr('height', d => d.y1 - d.y0)
              .attr('width', d => d.x1 - d.x0)
          )
      ,
      exit => exit.call(exit_ =>
        exit_
          .transition(t)
          .attr('fill-opacity', 0)
          .remove()
      )
    )

  svg
    .select('g.edges')
    .attr('fill', 'none')
    .selectAll('path')
    .data(links, d => d.source.production + d.source.sourceLinks.findIndex(l => l === d))
    .join(enter =>
      enter
        .append('path')
        .attr('d', d3.sankeyLinkHorizontal())
        .attr('stroke', d => d.source.color)
        .attr('stroke-width', d => Math.max(1, d.width))
        .attr('stroke-opacity', 0)
        .call(enter_ =>
          enter_
            .transition(t)
            .attr('stroke-opacity', 0.5)
        )
        .append('title').text(d => d.title)
      ,
      update => update
        .call(update_ =>
          update_
            .transition(t)
            .attr('d', d3.sankeyLinkHorizontal())
            .attr('stroke-width', d => Math.max(1, d.width))
        )
      ,
      exit => exit.call(exit_ =>
        exit_
          .transition(t)
          .attr('stroke-opacity', 0)
          .remove()
      )
    )
    .style('mix-blend-mode', 'multiply');

  svg
    .select('g.texts')
    .attr('font-family', 'sans-serif')
    .attr('font-size', 10)
    .selectAll('text')
    .data(nodes, d => d.title)
    .join(
      enter =>
        enter
          .append('text')
          .attr('x', d => (d.x0 < width / 2 ? d.x1 + 6 : d.x0 - 6))
          .attr('y', d => (d.y1 + d.y0) / 2)
          .attr('text-anchor', d => (d.x0 < width / 2 ? 'start' : 'end'))
          .attr('dy', '0.35em')
          .attr('fill-opacity', 0)
          .text(d => d.title)
          .call(enter_ =>
            enter_
              .transition(t)
              .attr('fill-opacity', 1)
          )
      ,
      update =>
      update.call(update_ =>
        update_
          .transition(t)
          .attr('fill-opacity', 1)
          .attr('x', d => (d.x0 < width / 2 ? d.x1 + 6 : d.x0 - 6))
          .attr('y', d => (d.y1 + d.y0) / 2)
          .attr('text-anchor', d => (d.x0 < width / 2 ? 'start' : 'end'))
      )
      ,
      exit => exit.call(exit_ =>
        exit_
          .transition(t)
          .attr('fill-opacity', 0)
          .remove()
      ))

});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
