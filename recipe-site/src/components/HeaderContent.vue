<template>
  <div class="header">
    <el-breadcrumb separator="/">
      <slot></slot>
    </el-breadcrumb>
    <div>
      <slot name="right"></slot>
    </div>
  </div>
</template>

<script>
export default {
  name: "HeaderContent",
}
</script>

<style>
.header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}
</style>
